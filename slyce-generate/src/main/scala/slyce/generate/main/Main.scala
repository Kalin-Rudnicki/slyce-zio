package slyce.generate.main

import klib.utils.*
import klib.utils.commandLine.parse.*
import zio.*

import slyce.generate.*

object Main extends ExecutableApp {

  private object generate {

    final case class SingleConfig(
        lexerFile: String,
        grammarFile: String,
        outputFile: String,
        targetLanguage: Option[TargetLanguage],
    )
    object SingleConfig {

      val parser: Parser[SingleConfig] = {
        Parser.singleValue[String]("lexer-file").required >&>
          Parser.singleValue[String]("grammar-file").required >&>
          Parser.singleValue[String]("output-file").required >&>
          Parser.singleValue.enumValues("target-language", TargetLanguage.values).optional
      }.map(SingleConfig.apply)

    }

    private def fileFromPath(path: String): SKTask[File] =
      File.fromPath(path).flatMap { file =>
        file.ensureExists *>
          file.isFile.flatMap {
            case true  => ZIO.succeed(file)
            case false => ZIO.failNEL(KError.UserError(s"Not a file: $path"))
          }
      }

    private val single: Executable =
      Executable
        .fromParser(SingleConfig.parser.disallowExtras)
        .withLayer(_ => ZIO.unit.toLayer)
        .withExecute { config =>
          for {
            _ <- Logger.println.info("Running generate/single")
            lexerFile <- fileFromPath(config.lexerFile)
            grammarFile <- fileFromPath(config.grammarFile)
            outputFile <- File.fromPath(config.outputFile)
            targetLanguage <- ZIO.fromOptionKError(TargetLanguage.parse(config.targetLanguage, outputFile.fileName.ext))(KError.UserError("Unable to assume target language"))
            _ <- Logger.println.info(targetLanguage)
          } yield ()
        }

    val executable: Executable =
      Executable.fromSubCommands(
        "single" -> single,
      )

  }

  override val executable: Executable =
    Executable.fromSubCommands(
      "generate" -> generate.executable,
    )

}
