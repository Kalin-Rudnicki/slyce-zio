package slyce.generate.main

import oxygen.executable.OxygenLoggerDefaults
import oxygen.predef.core.*
import oxygen.predef.executable.{*, given}
import oxygen.predef.zio.*
import zio.*

import slyce.core.*
import slyce.generate.*
import slyce.generate.error.GenerateError
import slyce.generate.parsers.Grammar as CurrentGrammar
import slyce.generate.parsers.Lexer as CurrentLexer

object Main extends ExecutableApp {

  override def oxygenLoggerDefaults: OxygenLoggerDefaults = OxygenLoggerDefaults.lean()

  private object generate {

    private def generate(
        lexerFile: Path,
        grammarFile: Path,
        outputFile: Path,
        targetLanguage: TargetLanguage,
        pkg: List[String],
    ): IO[GenerateError, Unit] = {
      val name = outputFile.fileName.baseName

      val lexerEffect: IO[GenerateError, CurrentLexer.NonTerminal.Lexer] =
        for {
          _ <- ZIO.logInfo("--- slf ---")
          lexerSource <- Helpers.sourceFromFile(lexerFile).mapError(GenerateError.Unexpected(_))
          _ <- ZIO.logInfo("tokenizing")
          lexerTokens <- Helpers.validatedToHTask(CurrentLexer.lexer.tokenize(lexerSource))
          // _ <- ZIO.logInfo(Source.markAll(lexerTokens.map(Token.mark)))
          _ <- ZIO.logInfo("building parse tree")
          lexerAST <- Helpers.validatedToHTask(CurrentLexer.grammar.buildTree(lexerSource, lexerTokens))
        } yield lexerAST

      val grammarEffect: IO[GenerateError, CurrentGrammar.NonTerminal.Grammar] =
        for {
          _ <- ZIO.logInfo("--- sgf ---")
          grammarSource <- Helpers.sourceFromFile(grammarFile).mapError(GenerateError.Unexpected(_))
          _ <- ZIO.logInfo("tokenizing")
          grammarTokens <- Helpers.validatedToHTask(CurrentGrammar.lexer.tokenize(grammarSource))
          // _ <- ZIO.logInfo(Source.markAll(lexerTokens.map(Token.mark)))
          _ <- ZIO.logInfo("building parse tree")
          grammarAST <- Helpers.validatedToHTask(CurrentGrammar.grammar.buildTree(grammarSource, grammarTokens))
        } yield grammarAST

      // TODO (KR) : indent? (TY self, fkin useless comment)
      for {
        _ <- ZIO.logInfo(s"Generating : $name")
        lexerAST <- lexerEffect
        grammarAST <- grammarEffect

        lexerInput = ConvertLexer.convertLexer(lexerAST)
        grammarInput = ConvertGrammar.convertGrammar(grammarAST)
        _ <- ZIO.logInfo("--- result ---")
        result <- Helpers.validatedToHTask(output.Result.build(lexerInput, grammarInput))
        resultString = output.formatters.Formatter.format(targetLanguage, pkg, name, result)

        _ <- ZIO.logInfo("--- output ---")
        outputParent <- ZIO.attempt { outputFile.parentOption.get }.mapError(GenerateError.Unexpected(_))
        _ <- outputParent.createDirectories.mapError(GenerateError.Unexpected(_))

        _ <- outputFile.write(resultString).mapError(GenerateError.Unexpected(_))
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
        Params.value[String]("lexer-file") &&
        Params.value[String]("grammar-file") &&
        Params.value[String]("output-file") &&
        Params.`enum`[TargetLanguage]("target-language").optional &&
        Params.value[String]("pkg").repeated
      }.map(SingleConfig.apply)

    }

    private val single: Executable =
      Executable
        .withCLIParser(SingleConfig.parser)
        .withExecute { config =>
          def fileFromPath(path: String): IO[GenerateError, Path] =
            Path.of(path).tap { _.assertExists(Path.Type.File) }.mapError(GenerateError.Unexpected(_))

          for {
            _ <- ZIO.logInfo("Running generate/single")

            lexerFile <- fileFromPath(config.lexerFile)
            grammarFile <- fileFromPath(config.grammarFile)
            outputFile <- Path.of(config.outputFile).mapError(GenerateError.Unexpected(_))
            targetLanguage <- TargetLanguage.parse(config.targetLanguage, outputFile.fileName.`extension`) match {
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

      val parser: Params[ForSrcDirConfig] = {
        Params.value[String]("src-file", hints = List("your project: 'a/b/c/src/main/scala/com/xyz' -> '--source-file=a/b/c/src'")) &&
        Params.`enum`[TargetLanguage]("target-language").withDefault(TargetLanguage.Scala3) &&
        Params.flag("snapshot")
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
        .withCLIParser(ForSrcDirConfig.parser)
        .withExecute { config =>
          def dirFromPath(path: String): IO[GenerateError, Path] =
            Path.of(path).tap { _.assertExists(Path.Type.Directory) }.mapError(GenerateError.Unexpected(_))

          def findEntries(
              dir: Path,
              pkg: List[String],
          ): IO[GenerateError, List[Entry]] =
            for {
              _ <- ZIO.logDebug(s"Searching in: $dir")
              children <- dir.children.mapBoth(GenerateError.Unexpected(_), _.toList)
              fileChildren <- ZIO.filter(children)(_.`type`.map(_ == Path.Type.File)).mapError(GenerateError.Unexpected(_))
              dirChildren <- ZIO.filter(children)(_.`type`.map(_ == Path.Type.Directory)).mapError(GenerateError.Unexpected(_))

              fileEntries = fileChildren.groupBy(_.fileName.baseName).toList.flatMap { case (baseName, files) =>
                files.map(f => (f, f.fileName.`extension`)).sortBy(_._2) match {
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
              dirEntries <- ZIO.foreach(dirChildren) { d =>
                findEntries(d, pkg :+ d.fileName.baseName)
              }
            } yield (fileEntries :: dirEntries).flatten

          for {
            _ <- ZIO.logInfo("Running generate/src-dir")

            srcDir <- dirFromPath(config.srcFile)
            extName = TargetLanguage.extName(config.targetLanguage)
            slyceRoot = srcDir.resolve("main/slyce")
            _ <- slyceRoot.assertExists(Path.Type.Directory).mapError(GenerateError.Unexpected(_))
            srcRoot = srcDir.resolve(s"main/$extName")
            tail = if config.snapshot then "Snapshot" else ""

            entries <- findEntries(slyceRoot, Nil)
            _ <- ZIO.foreachDiscard(entries) { entry =>
              val outputFile = srcRoot.resolve((entry.pkg :+ s"${entry.baseName}$tail.$extName").mkString("/"))
              generate(entry.lexerFile, entry.grammarFile, outputFile, config.targetLanguage, entry.pkg)
            }
          } yield ()
        }

    val executable: Executable =
      Executable.oneOf(
        "single" -> single,
        "src-dir" -> forSrcDir,
      )

  }

  override val executable: Executable =
    Executable.oneOf(
      "generate" -> generate.executable,
    )

}
