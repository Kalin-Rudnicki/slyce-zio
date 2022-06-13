package slyce.generate.main

import klib.utils.*
import klib.utils.commandLine.parse.*
import zio.*

import slyce.core.*
import slyce.generate.*

object Main extends ExecutableApp {

  private object generate {

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

    private def fileFromPath(path: String): SKTask[File] =
      File.fromPath(path).flatMap { file =>
        file.ensureExists *>
          file.isFile.flatMap {
            case true  => ZIO.succeed(file)
            case false => ZIO.failNEL(KError.UserError(s"Not a file: $path"))
          }
      }

    private def convertValidated[A](validated: Validated[A]): KTask[A] =
      validated match {
        case Left(errors) => ZIO.fail(errors.map(e => KError.UserError(e.toString)))
        case Right(value) => ZIO.succeed(value)
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

            _ <- Logger.println.info("--- slf ---")
            lexerSource <- Source.fromFile(lexerFile)
            _ <- Logger.println.info("tokenizing")
            lexerTokens <- convertValidated(parsers.Lexer2.lexer.tokenize(lexerSource))
            _ <- Logger.println.info("building parse tree")
            lexerAST <- convertValidated(parsers.Lexer2.grammar.buildTree(lexerSource, lexerTokens))

            _ <- Logger.println.info("--- sgf ---")
            grammarSource <- Source.fromFile(grammarFile)
            _ <- Logger.println.info("tokenizing")
            grammarTokens <- convertValidated(parsers.Grammar2.lexer.tokenize(grammarSource))
            _ <- Logger.println.info("building parse tree")
            grammarAST <- convertValidated(parsers.Grammar2.grammar.buildTree(grammarSource, grammarTokens))

            lexerInput = ConvertLexer.convertLexer(lexerAST)
            grammarInput = ConvertGrammar.convertGrammar(grammarAST)
            _ <- Logger.println.info("--- result ---")
            result <- convertValidated(output.Result.build(lexerInput, grammarInput))
            resultString = output.formatters.Formatter.format(targetLanguage, config.pkg, outputFile.fileName.base, result)

            _ <- Logger.println.info("--- output ---")
            _ <- outputFile.writeString(resultString)
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
