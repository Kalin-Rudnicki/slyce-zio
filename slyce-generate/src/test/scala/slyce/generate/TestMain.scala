package slyce.generate

import cats.syntax.either.*
import cats.syntax.option.*
import harness.cli.*
import harness.core.{given, *}
import harness.zio.*
import zio.*

import slyce.core.*
import slyce.generate.builder.Builders.*
import slyce.generate.debugging.Result as DebugResult
import slyce.generate.grammar.*
import slyce.generate.lexer.*
import slyce.generate.output.*

object TestMain extends ExecutableApp {

  private[generate] def makeExe(
      name: String,
      lexerInput: LexerInput,
      grammarInput: GrammarInput,
      baseDir: List[String] = List(".", "slyce-generate", "src", "test", "scala"),
      pkg: List[String] = List("slyce", "generate", "test"),
  ): (String, Executable) =
    (
      name,
      Executable
        .withParser(Parser.unit)
        .withEffect { _ =>
          for {
            _ <- Logger.log.info(s"=====| TestMain : $name |=====")

            debugResult = DebugResult.build(lexerInput, grammarInput)
            debugResultFrag = DebugResult.resultToHTML(debugResult)
            debugResultString = debugResultFrag.render

            debugOutputFile <- Path("target/test-output.html")
            _ <- debugOutputFile.writeString(debugResultString)

            refName = name.split("-").map(_.capitalize).mkString
            result <- Result.build(lexerInput, grammarInput) match {
              case Right(value) => ZIO.succeed(value)
              case Left(errors) => ZIO.fail(HError(errors.map(e => HError.UserError(e.toString))))
            }
            resultString = formatters.scala3.Scala3Formatter.format(pkg, refName, result)
            // _ <- Logger.log.info(resultString)
            _ <- Logger.log.info(s"Generated ${resultString.count(_ == '\n')} line(s)")

            _ <- Logger.log("")
            dirPath = (baseDir ::: pkg).mkString("/")
            dirFile <- Path(dirPath)
            _ <- dirFile.mkdirs
            outFile <- dirFile.child(s"$refName.scala")
            _ <- Logger.log.info(s"writing to: ${outFile.canonicalPath.show}")
            _ <- outFile.writeString(resultString)
          } yield ()
        },
    )

  override val executable: Executable =
    Executable.fromSubCommands(
    )

}
