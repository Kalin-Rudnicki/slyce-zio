package slyce.generate.main

import cats.syntax.option.*
import harness.cli.*
import harness.core.{given, *}
import harness.zio.*
import zio.*

import slyce.core.*
import slyce.generate.*
import slyce.generate.parsers.Grammar as CurrentGrammar
import slyce.generate.parsers.Lexer as CurrentLexer

object Main extends ExecutableApp {

  private object generate {

    private def generate(
        lexerFile: Path,
        grammarFile: Path,
        outputFile: Path,
        targetLanguage: TargetLanguage,
        pkg: List[String],
    ): SHTask[Unit] = {
      val name = outputFile.pathName.base

      val lexerEffect: SHTask[CurrentLexer.NonTerminal.Lexer] =
        for {
          _ <- Logger.log.info("--- slf ---")
          lexerSource <- Source.fromFile(lexerFile)
          _ <- Logger.log.info("tokenizing")
          lexerTokens <- Validated.toHTask(CurrentLexer.lexer.tokenize(lexerSource))
          // _ <- Logger.log.info(Source.markAll(lexerTokens.map(Token.mark)))
          _ <- Logger.log.info("building parse tree")
          lexerAST <- Validated.toHTask(CurrentLexer.grammar.buildTree(lexerSource, lexerTokens))
        } yield lexerAST

      val grammarEffect: SHTask[CurrentGrammar.NonTerminal.Grammar] =
        for {
          _ <- Logger.log.info("--- sgf ---")
          grammarSource <- Source.fromFile(grammarFile)
          _ <- Logger.log.info("tokenizing")
          grammarTokens <- Validated.toHTask(CurrentGrammar.lexer.tokenize(grammarSource))
          // _ <- Logger.log.info(Source.markAll(lexerTokens.map(Token.mark)))
          _ <- Logger.log.info("building parse tree")
          grammarAST <- Validated.toHTask(CurrentGrammar.grammar.buildTree(grammarSource, grammarTokens))
        } yield grammarAST

      // TODO (KR) : indent?
      for {
        _ <- Logger.log.info(s"Generating : $name")
        (lexerAST, grammarAST) <- lexerEffect <**> grammarEffect

        lexerInput = ConvertLexer.convertLexer(lexerAST)
        grammarInput = ConvertGrammar.convertGrammar(grammarAST)
        _ <- Logger.log.info("--- result ---")
        result <- Validated.toHTask(output.Result.build(lexerInput, grammarInput))
        resultString = output.formatters.Formatter.format(targetLanguage, pkg, name, result)

        _ <- Logger.log.info("--- output ---")
        outputParent <- outputFile.parent
        _ <- outputParent.mkdirs

        _ <- outputFile.writeString(resultString)
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
          def fileFromPath(path: String): SHTask[Path] =
            Path(path).flatMap { file =>
              file.ensureExists *>
                file.isFile.flatMap {
                  case true  => ZIO.succeed(file)
                  case false => ZIO.fail(HError.UserError(s"Not a file: $path"))
                }
            }

          for {
            _ <- Logger.log.info("Running generate/single")

            lexerFile <- fileFromPath(config.lexerFile)
            grammarFile <- fileFromPath(config.grammarFile)
            outputFile <- Path(config.outputFile)
            targetLanguage <- TargetLanguage.parse(config.targetLanguage, outputFile.pathName.ext) match {
              case Some(value) => ZIO.succeed(value)
              case None        => ZIO.fail(HError.UserError("Unable to assume target language"))
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
          def dirFromPath(path: String): SHTask[Path] =
            Path(path).flatMap(ensureDir)

          def ensureDir(file: Path): SHTask[Path] =
            file.ensureExists *>
              file.isDirectory.flatMap {
                case true  => ZIO.succeed(file)
                case false => ZIO.fail(HError.UserError(s"Not a directory: $file"))
              }

          def findEntries(
              dir: Path,
              pkg: List[String],
          ): SHTask[List[Entry]] =
            for {
              _ <- Logger.log.debug(s"Searching in: $dir")
              children <- dir.children.map(_.toList)
              fileChildren <- ZIO.filter(children)(_.isFile)
              dirChildren <- ZIO.filter(children)(_.isDirectory)

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
            slyceRoot <- srcDir.child("main/slyce").flatMap(ensureDir)
            srcRoot <- srcDir.child(s"main/$extName")
            tail = if (config.snapshot) "Snapshot" else ""

            entries <- findEntries(slyceRoot, Nil)
            _ <- ZIO.traverse(entries) { entry =>
              for {
                outputFile <- srcRoot.child((entry.pkg :+ s"${entry.baseName}$tail.$extName").mkString("/"))
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
