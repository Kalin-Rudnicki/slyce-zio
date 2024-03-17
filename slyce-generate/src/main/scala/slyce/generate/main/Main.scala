package slyce.generate.main

import cats.syntax.option.*
import harness.cli.*
import harness.core.{given, *}
import harness.zio.*
import zio.*

import slyce.core.*
import slyce.generate.*
import slyce.generate.error.GenerateError
import slyce.generate.parsers.Grammar as CurrentGrammar
import slyce.generate.parsers.Lexer as CurrentLexer

object Main extends ExecutableApp {

  private implicit val errorLogger: ErrorLogger[GenerateError] =
    ErrorLogger.withGetMessage[GenerateError].atLevel.fatal

  private object generate {

    private def generate(
        lexerFile: Path,
        grammarFile: Path,
        outputFile: Path,
        targetLanguage: TargetLanguage,
        pkg: List[String],
    ): ZIO[HarnessEnv, GenerateError, Unit] = {
      val name = outputFile.pathName.base

      val lexerEffect: ZIO[HarnessEnv, GenerateError, CurrentLexer.NonTerminal.Lexer] =
        for {
          _ <- Logger.log.info("--- slf ---")
          lexerSource <- Helpers.sourceFromFile(lexerFile).mapError(GenerateError.Unexpected(_))
          _ <- Logger.log.info("tokenizing")
          lexerTokens <- Helpers.validatedToHTask(CurrentLexer.lexer.tokenize(lexerSource))
          // _ <- Logger.log.info(Source.markAll(lexerTokens.map(Token.mark)))
          _ <- Logger.log.info("building parse tree")
          lexerAST <- Helpers.validatedToHTask(CurrentLexer.grammar.buildTree(lexerSource, lexerTokens))
        } yield lexerAST

      val grammarEffect: ZIO[HarnessEnv, GenerateError, CurrentGrammar.NonTerminal.Grammar] =
        for {
          _ <- Logger.log.info("--- sgf ---")
          grammarSource <- Helpers.sourceFromFile(grammarFile).mapError(GenerateError.Unexpected(_))
          _ <- Logger.log.info("tokenizing")
          grammarTokens <- Helpers.validatedToHTask(CurrentGrammar.lexer.tokenize(grammarSource))
          // _ <- Logger.log.info(Source.markAll(lexerTokens.map(Token.mark)))
          _ <- Logger.log.info("building parse tree")
          grammarAST <- Helpers.validatedToHTask(CurrentGrammar.grammar.buildTree(grammarSource, grammarTokens))
        } yield grammarAST

      // TODO (KR) : indent? (TY self, fkin useless comment)
      for {
        _ <- Logger.log.info(s"Generating : $name")
        lexerAST <- lexerEffect
        grammarAST <- grammarEffect

        lexerInput = ConvertLexer.convertLexer(lexerAST)
        grammarInput = ConvertGrammar.convertGrammar(grammarAST)
        _ <- Logger.log.info("--- result ---")
        result <- Helpers.validatedToHTask(output.Result.build(lexerInput, grammarInput))
        resultString = output.formatters.Formatter.format(targetLanguage, pkg, name, result)

        _ <- Logger.log.info("--- output ---")
        outputParent <- outputFile.parent.mapError(GenerateError.Unexpected(_))
        _ <- outputParent.mkdirs.mapError(GenerateError.Unexpected(_))

        _ <- outputFile.writeString(resultString).mapError(GenerateError.Unexpected(_))
      } yield ()
    }

    final case class SingleConfig(
        lexerFile: String,
        grammarFile: String,
        outputFile: String,
        targetLanguage: Option[TargetLanguage],
        pkg: List[String],
    )
    object SingleConfig {

      val parser: Parser[SingleConfig] = {
        Parser.value[String](LongName.unsafe("lexer-file")) &&
        Parser.value[String](LongName.unsafe("grammar-file")) &&
        Parser.value[String](LongName.unsafe("output-file")) &&
        Parser.value.`enum`[TargetLanguage, String](LongName.unsafe("target-language")).optional &&
        Parser.values.list[String](LongName.unsafe("pkg"))
      }.map(SingleConfig.apply)

    }

    private val single: Executable =
      Executable
        .withParser(SingleConfig.parser)
        .withEffect { config =>
          def fileFromPath(path: String): ZIO[HarnessEnv, GenerateError, Path] =
            Path(path).tap { _.ensureIsFile }.mapError(GenerateError.Unexpected(_))

          for {
            _ <- Logger.log.info("Running generate/single")

            lexerFile <- fileFromPath(config.lexerFile)
            grammarFile <- fileFromPath(config.grammarFile)
            outputFile <- Path(config.outputFile).mapError(GenerateError.Unexpected(_))
            targetLanguage <- TargetLanguage.parse(config.targetLanguage, outputFile.pathName.ext) match {
              case Some(value) => ZIO.succeed(value)
              case None        => ZIO.fail(GenerateError.InvalidInput("Unable to assume target language"))
            }

            _ <- generate(lexerFile, grammarFile, outputFile, targetLanguage, config.pkg)
          } yield ()
        }

    final case class ForSrcDirConfig(
        srcFile: String,
        targetLanguage: TargetLanguage,
        snapshot: Boolean,
    )
    object ForSrcDirConfig {

      val parser: Parser[ForSrcDirConfig] = {
        Parser.value[String](LongName.unsafe("src-file")) &&
        Parser.value.`enum`[TargetLanguage, String](LongName.unsafe("target-language")).default(TargetLanguage.Scala3) &&
        Parser.flag(LongName.unsafe("snapshot"), shortParam = Defaultable.Some(ShortName.unsafe('S')))
      }.map(ForSrcDirConfig.apply)

    }

    private final case class Entry(
        baseName: String,
        lexerFile: Path,
        grammarFile: Path,
        pkg: List[String],
    )

    private val forSrcDir: Executable =
      Executable
        .withParser(ForSrcDirConfig.parser)
        .withEffect { config =>
          def dirFromPath(path: String): ZIO[HarnessEnv, GenerateError, Path] =
            Path(path).tap { _.ensureIsDirectory }.mapError(GenerateError.Unexpected(_))

          def findEntries(
              dir: Path,
              pkg: List[String],
          ): ZIO[HarnessEnv, GenerateError, List[Entry]] =
            for {
              _ <- Logger.log.debug(s"Searching in: $dir")
              children <- dir.children.mapBoth(GenerateError.Unexpected(_), _.toList)
              fileChildren <- ZIO.filter(children)(_.isFile).mapError(GenerateError.Unexpected(_))
              dirChildren <- ZIO.filter(children)(_.isDirectory).mapError(GenerateError.Unexpected(_))

              fileEntries = fileChildren.groupBy(_.pathName.base).toList.flatMap { case (baseName, files) =>
                files.map(f => (f, f.pathName.ext)).sortBy(_._2) match {
                  case List((grammarFile, Some("sgf")), (lexerFile, Some("slf"))) =>
                    Entry(
                      baseName = baseName,
                      lexerFile = lexerFile,
                      grammarFile = grammarFile,
                      pkg = pkg,
                    ).some
                  case _ =>
                    None
                }
              }
              dirEntries <- ZIO.traverse(dirChildren) { d =>
                findEntries(d, pkg :+ d.pathName.name)
              }
            } yield (fileEntries :: dirEntries).flatten

          for {
            _ <- Logger.log.info("Running generate/src-dir")

            srcDir <- dirFromPath(config.srcFile)
            extName = TargetLanguage.extName(config.targetLanguage)
            slyceRoot <- srcDir.child("main/slyce").tap(_.ensureIsDirectory).mapError(GenerateError.Unexpected(_))
            srcRoot <- srcDir.child(s"main/$extName").mapError(GenerateError.Unexpected(_))
            tail = if (config.snapshot) "Snapshot" else ""

            entries <- findEntries(slyceRoot, Nil)
            _ <- ZIO.traverse(entries) { entry =>
              for {
                outputFile <- srcRoot.child((entry.pkg :+ s"${entry.baseName}$tail.$extName").mkString("/")).mapError(GenerateError.Unexpected(_))
                _ <- generate(entry.lexerFile, entry.grammarFile, outputFile, config.targetLanguage, entry.pkg)
              } yield ()
            }
          } yield ()
        }

    val executable: Executable =
      Executable.fromSubCommands(
        "single" -> single,
        "src-dir" -> forSrcDir,
      )

  }

  override val executable: Executable =
    Executable.fromSubCommands(
      "generate" -> generate.executable,
    )

}
