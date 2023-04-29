package slyce.generate

import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.{given, *}
import harness.core.commandLine.parse.*
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
        .fromParser(Parser.unit.disallowExtras)
        .withLayer { _ => ZIO.unit.toLayer }
        .withExecute { _ =>
          for {
            _ <- Logger.println.info(s"=====| TestMain : $name |=====")

            debugResult = DebugResult.build(lexerInput, grammarInput)
            debugResultFrag = DebugResult.resultToHTML(debugResult)
            debugResultString = debugResultFrag.render

            debugOutputFile <- File.fromPath("target/test-output.html")
            _ <- debugOutputFile.writeString(debugResultString)

            refName = name.split("-").map(_.capitalize).mkString
            result <- ZIO.fromEither(Result.build(lexerInput, grammarInput).leftMap(_.map(e => KError.UserError(e.toString))))
            resultString = formatters.scala3.Scala3Formatter.format(pkg, refName, result)
            // _ <- Logger.println.info(resultString)
            _ <- Logger.println.info(s"Generated ${resultString.count(_ == '\n')} line(s)")

            _ <- Logger.break()
            dirPath = (baseDir ::: pkg).mkString("/")
            dirFile <- File.fromPath(dirPath)
            _ <- dirFile.createDirectories()
            outFile <- dirFile.child(s"$refName.scala")
            _ <- Logger.println.info(s"writing to: ${outFile.toJavaFile.getCanonicalPath}")
            _ <- outFile.writeString(resultString)
          } yield ()
        },
    )

  override val executable: Executable =
    Executable.fromSubCommands(
    )

}
