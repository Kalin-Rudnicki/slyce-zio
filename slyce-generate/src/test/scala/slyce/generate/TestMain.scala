package slyce.generate

import cats.syntax.option.*
import klib.utils.*
import klib.utils.commandLine.parse.*
import zio.*

import slyce.generate.builder.Builders.*
import slyce.generate.debugging.Result
import slyce.generate.lexer.*

object TestMain extends ExecutableApp {

  override val executable: Executable =
    Executable
      .fromParser(Parser.unit.disallowExtras)
      .withLayer { _ => ZIO.unit.toLayer }
      .withExecute { _ =>
        for {
          _ <- Logger.println.info("=====| TestMain |=====")
          lexerInput =
            lexer("test")(
              lexer.mode("test")(
                lexer.mode.line(Regex.CharClass.inclusive('A').repeat(2, 5.some))(
                  Yields.Yield.Terminal("text"),
                ),
                lexer.mode.line(Regex.Sequence("AA"))(),
                lexer.mode.line(Regex.Sequence("AAAA"))(),
              ),
            )
          result = Result.build(lexerInput)
          resultFrag = Result.resultToHTML(result)
          resultString = resultFrag.render
          outputFile <- File.fromPath("test-output.html")
          _ <- outputFile.writeString(resultString)
        } yield ()
      }

}
