package slyce.generate.lexer

import cats.syntax.either.*
import cats.syntax.option.*
import zio.test.*
import zio.test.Assertion.*

import slyce.core.*
import slyce.generate.builder.Builders.*

object DFATests extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("DFATests")(
      suite("basic successes")(
      ),
      suite("basic failures")(
        testDFA("invalid start mode")(
          lexer("test")(
            lexer.mode("test-2")(
              lexer.mode.line(Regex.Sequence("A"))(),
            ),
          ),
          1.some,
        ),
        testDFA("invalid to-mode")(
          lexer("test")(
            lexer.mode("test")(
              lexer.mode.line(Regex.Sequence("A"), Yields.ToMode.Push("test-2"))(),
            ),
          ),
          1.some,
        ),
        testDFA("shadows-basic")(
          lexer("test")(
            lexer.mode("test")(
              lexer.mode.line(Regex.Sequence("ABC"))(),
              lexer.mode.line(Regex.Sequence("ABC"))(),
            ),
          ),
          1.some,
        ),
        testDFA("shadows-many:1")(
          lexer("test")(
            lexer.mode("test")(
              lexer.mode.line(Regex.Sequence("ABC"))(),
              lexer.mode.line(Regex.Sequence("ABC"))(),
              lexer.mode.line(Regex.Sequence("ABC"))(),
            ),
          ),
          2.some,
        ),
        testDFA("shadows-many:many")(
          lexer("test")(
            lexer.mode("test")(
              lexer.mode.line(Regex.Sequence("ABC"))(),
              lexer.mode.line(Regex.Sequence("ABC"))(),
              lexer.mode.line(Regex.Sequence("DEF"))(),
              lexer.mode.line(Regex.Sequence("DEF"))(),
            ),
          ),
          2.some,
        ),
        testDFA("shadows-non-exact")(
          lexer("test")(
            lexer.mode("test")(
              lexer.mode.line(Regex.CharClass.inclusive('A').repeat(1, 5.some))(),
              lexer.mode.line(Regex.Sequence("AA"))(),
              lexer.mode.line(Regex.Sequence("AAAA"))(),
            ),
          ),
          2.some,
        ),
      ),
    )

}
