package slyce.generate.main

import cats.syntax.option.*
import klib.utils.{given, *}
import klib.utils.commandLine.parse.*
import zio.*

import slyce.core.*
import slyce.generate.*

object Main extends ExecutableApp {

  private object generate {

    private def generate(
        lexerFile: File,
        grammarFile: File,
        outputFile: File,
        targetLanguage: TargetLanguage,
        pkg: List[String],
    ): SKTask[Unit] = {
      val name = outputFile.fileName.base

      val lexerEffect: SKTask[parsers.Lexer.NonTerminal.Lexer] =
        for {
          _ <- Logger.println.info("--- slf ---")
          lexerSource <- Source.fromFile(lexerFile)
          _ <- Logger.println.info("tokenizing")
          lexerTokens <- Validated.toKTask(parsers.Lexer.lexer.tokenize(lexerSource))
          _ <- Logger.println.info("building parse tree")
          lexerAST <- Validated.toKTask(parsers.Lexer.grammar.buildTree(lexerSource, lexerTokens))
        } yield lexerAST

      val grammarEffect: SKTask[parsers.Grammar.NonTerminal.Grammar] =
        for {
          _ <- Logger.println.info("--- sgf ---")
          grammarSource <- Source.fromFile(grammarFile)
          _ <- Logger.println.info("tokenizing")
          grammarTokens <- Validated.toKTask(parsers.Grammar.lexer.tokenize(grammarSource))
          _ <- Logger.println.info("building parse tree")
          grammarAST <- Validated.toKTask(parsers.Grammar.grammar.buildTree(grammarSource, grammarTokens))
        } yield grammarAST

      Logger.println.info(s"Generating : $name") *>
        Logger.withIndent(1) {
          for {
            (lexerAST, grammarAST) <- lexerEffect <**> grammarEffect

            lexerInput = ConvertLexer.convertLexer(lexerAST)
            grammarInput = ConvertGrammar.convertGrammar(grammarAST)
            _ <- Logger.println.info("--- result ---")
            result <- Validated.toKTask(output.Result.build(lexerInput, grammarInput))
            resultString = output.formatters.Formatter.format(targetLanguage, pkg, name, result)

            _ <- Logger.println.info("--- output ---")
            outputParent <- ZIO.kAttempt("Failed to get parent of output file")(File.fromNIOPath(outputFile.toPath.getParent))
            _ <- outputParent.createDirectories()

            _ <- outputFile.writeString(resultString)
          } yield ()
        }
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
        Parser.singleValue[String]("lexer-file").required >&>
          Parser.singleValue[String]("grammar-file").required >&>
          Parser.singleValue[String]("output-file").required >&>
          Parser.singleValue.enumValues("target-language", TargetLanguage.values).optional >&>
          Parser.singleValue[String]("pkg").many.required
      }.map(SingleConfig.apply)

    }

    private val single: Executable =
      Executable
        .fromParser(SingleConfig.parser.disallowExtras)
        .withLayer(_ => ZIO.unit.toLayer)
        .withExecute { config =>
          def fileFromPath(path: String): SKTask[File] =
            File.fromPath(path).flatMap { file =>
              file.ensureExists *>
                file.isFile.flatMap {
                  case true  => ZIO.succeed(file)
                  case false => ZIO.failNEL(KError.UserError(s"Not a file: $path"))
                }
            }

          for {
            _ <- Logger.println.info("Running generate/single")

            lexerFile <- fileFromPath(config.lexerFile)
            grammarFile <- fileFromPath(config.grammarFile)
            outputFile <- File.fromPath(config.outputFile)
            targetLanguage <- ZIO.fromOptionKError(TargetLanguage.parse(config.targetLanguage, outputFile.fileName.ext))(KError.UserError("Unable to assume target language"))

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
        Parser.singleValue[String]("src-file").required >&>
          Parser.singleValue.enumValues("target-language", TargetLanguage.values).default(TargetLanguage.Scala3) >&>
          Parser.flag("snapshot", primaryShortParamName = Defaultable.Some('S')).required
      }.map(ForSrcDirConfig.apply)

    }

    private final case class Entry(
        baseName: String,
        lexerFile: File,
        grammarFile: File,
        pkg: List[String],
    )

    private val forSrcDir: Executable =
      Executable
        .fromParser(ForSrcDirConfig.parser.disallowExtras)
        .withLayer(_ => ZIO.unit.toLayer)
        .withExecute { config =>
          def dirFromPath(path: String): SKTask[File] =
            File.fromPath(path).flatMap(ensureDir)

          def ensureDir(file: File): SKTask[File] =
            file.ensureExists *>
              file.isDirectory.flatMap {
                case true  => ZIO.succeed(file)
                case false => ZIO.failNEL(KError.UserError(s"Not a directory: $file"))
              }

          def findEntries(
              dir: File,
              pkg: List[String],
          ): SKTask[List[Entry]] =
            for {
              _ <- Logger.println.debug(s"Searching in: $dir")
              children <- dir.children.map(_.toList)
              fileChildren <- ZIO.filter(children)(_.isFile)
              dirChildren <- ZIO.filter(children)(_.isDirectory)

              fileEntries = fileChildren.toList.groupBy(_.fileName.base).toList.flatMap { (baseName, files) =>
                files.map(f => (f, f.fileName.ext)).sortBy(_._2) match {
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
              dirEntries <- ZIO.traverseNEL(dirChildren.toList) { d =>
                findEntries(d, pkg :+ d.fileName.name)
              }
            } yield (fileEntries :: dirEntries).flatten

          for {
            _ <- Logger.println.info("Running generate/src-dir")

            srcDir <- dirFromPath(config.srcFile)
            extName = TargetLanguage.extName(config.targetLanguage)
            slyceRoot <- srcDir.child("main/slyce").flatMap(ensureDir)
            srcRoot <- srcDir.child(s"main/$extName")
            tail = if (config.snapshot) "Snapshot" else ""

            entries <- findEntries(slyceRoot, Nil)
            _ <- ZIO.traverseNEL(entries) { entry =>
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
