package slyce.generate.lexer

import cats.syntax.either.*
import cats.syntax.option.*
import zio.test.*
import zio.test.Assertion.*

import slyce.core.*

object NFATests extends DefaultRunnableSpec {

  private def lexerInputFromRegex(reg: Regex): LexerInput =
    LexerInput(
      Marked("test", Span.Unknown),
      LexerInput.Mode(
        Marked("test", Span.Unknown),
        LexerInput.Mode.Line(
          1,
          Marked(reg, Span.Unknown),
          Yields(Nil, Marked(Yields.ToMode.Same, Span.Unknown)),
        ) :: Nil,
      ) :: Nil,
    )

  override def spec: ZSpec[TestEnvironment, Any] = {
    def testRegex(name: String)(reg: Regex, expNumErrors: Option[Int]): ZSpec[TestEnvironment, Any] =
      test(name) {
        val nfa = NFA.fromLexer(lexerInputFromRegex(reg))

        expNumErrors match {
          case Some(expNumErrors) => assert(nfa.leftMap(_.toList))(isLeft(hasSize(equalTo(expNumErrors))))
          case None               => assert(nfa)(isRight)
        }
      }

    suite("NFATests")(
      suite("basic successes")(
        testRegex("empty exclusive char class")(Regex.CharClass.exclusive(), None),
        testRegex("non-empty inclusive char class")(Regex.CharClass.inclusive('A'), None),
        testRegex("non-empty exclusive char class")(Regex.CharClass.exclusive('A'), None),
        testRegex("empty sequence")(Regex.Sequence(Nil), None),
        testRegex("non-empty sequence")(Regex.Sequence("ABC"), None),
        testRegex("group : 1 sequence")(
          Regex.Group(
            Regex.Sequence("ABC"),
          ),
          None,
        ),
        testRegex("group : many sequences")(
          Regex.Group(
            Regex.Sequence("ABC"),
            Regex.Sequence("DEF"),
            Regex.Sequence("GHI"),
          ),
          None,
        ),
        testRegex("repeat : 0 -> None")(Regex.CharClass.inclusive('A').repeat(0, None), None),
        testRegex("repeat : 2 -> None")(Regex.CharClass.inclusive('A').repeat(2, None), None),
        testRegex("repeat : 0 -> 2.some")(Regex.CharClass.inclusive('A').repeat(0, 2.some), None),
        testRegex("repeat : 2 -> 2.some")(Regex.CharClass.inclusive('A').repeat(2, 2.some), None),
        testRegex("repeat : 2 -> 4.some")(Regex.CharClass.inclusive('A').repeat(2, 4.some), None),
      ),
      suite("basic failures")(
        testRegex("empty inclusive char class")(Regex.CharClass.inclusive(), 1.some),
        testRegex("repeat : min < 0")(Regex.CharClass.inclusive('A').repeat(-1, None), 1.some),
        testRegex("repeat : max < min")(Regex.CharClass.inclusive('A').repeat(1, 0.some), 1.some),
        testRegex("repeat : min < 0, max < min")(Regex.CharClass.inclusive('A').repeat(-1, -2.some), 2.some),
        testRegex("repeat : min == max == 0")(Regex.CharClass.inclusive('A').repeat(0, 0.some), 1.some),
      ),
    )
  }

}
