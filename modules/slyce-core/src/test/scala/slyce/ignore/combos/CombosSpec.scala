package slyce.ignore.combos

import oxygen.predef.test.*

import slyce.TestUtils.*
import slyce.core.*

object CombosSpec extends OxygenSpecDefault {

  private def esc(s: String): String = s.replace("\n", "\\n").replace("\t", "\\t")

  /** parse `x , y` down to `(x, y)` BigInt pair, assert it equals `(1, 2)`. */
  private def ok(parse: Source => Either[?, (BigInt, BigInt)], input: String)(using Trace, SourceLocation): TestSpec =
    test(s"accept ${esc(input)}") {
      assert(parse(source(input)))(isRight(equalTo((BigInt(1), BigInt(2)))))
    }

  private def no(parse: Source => Either[?, Any], input: String)(using Trace, SourceLocation): TestSpec =
    test(s"reject ${esc(input)}") {
      assert(parse(source(input)))(isLeft)
    }

  // value-projection adapters
  private val before: Source => Either[?, (BigInt, BigInt)] = s => BeforePair.parser.parse(s).map(p => (p.x.value, p.y.value))
  private val between: Source => Either[?, (BigInt, BigInt)] = s => BetweenPair.parser.parse(s).map(p => (p.x.value, p.y.value))
  private val after: Source => Either[?, (BigInt, BigInt)] = s => AfterPair.parser.parse(s).map(p => (p.x.value, p.y.value))
  private val beforeAfter: Source => Either[?, (BigInt, BigInt)] = s => BeforeAfterPair.parser.parse(s).map(p => (p.x.value, p.y.value))
  private val betweenAfter: Source => Either[?, (BigInt, BigInt)] = s => BetweenAfterPair.parser.parse(s).map(p => (p.x.value, p.y.value))
  private val beforeBetween: Source => Either[?, (BigInt, BigInt)] = s => BeforeBetweenPair.parser.parse(s).map(p => (p.x.value, p.y.value))
  private val noNl: Source => Either[?, (BigInt, BigInt)] = s => NoNlPair.parser.parse(s).map(p => (p.x.value, p.y.value))
  private val comment: Source => Either[?, (BigInt, BigInt)] = s => CommentPair.parser.parse(s).map(p => (p.x.value, p.y.value))

  override def testSpec: TestSpec =
    suite("CombosSpec")(
      suite("@ignoreBefore only — leading ws only")(
        ok(before, "1,2"),
        ok(before, "  1,2"),
        ok(before, "\n1,2"),
        no(before, "1 ,2"), // ws after x
        no(before, "1, 2"), // ws after comma
        no(before, "1,2 "), // trailing
      ),
      suite("@ignoreBetween only — internal gaps only (the key case)")(
        ok(between, "1,2"),
        ok(between, "1 ,2"),  // after x
        ok(between, "1, 2"),  // after comma
        ok(between, "1 , 2"), // both internal gaps
        no(between, " 1,2"),  // leading
        no(between, "1,2 "),  // trailing
        no(between, " 1 , 2 "),
      ),
      suite("@ignoreAfter only — trailing ws only")(
        ok(after, "1,2"),
        ok(after, "1,2 "),
        ok(after, "1,2\n"),
        no(after, " 1,2"), // leading
        no(after, "1 ,2"), // internal
        no(after, "1, 2"),
      ),
      suite("@ignoreBefore + @ignoreAfter — edges only, no internal")(
        ok(beforeAfter, "1,2"),
        ok(beforeAfter, "  1,2  "),
        no(beforeAfter, "1 ,2"),
        no(beforeAfter, "1, 2"),
      ),
      suite("@ignoreBetween + @ignoreAfter — internal + trailing, no leading")(
        ok(betweenAfter, "1 , 2 "),
        ok(betweenAfter, "1,2"),
        no(betweenAfter, " 1,2"),
      ),
      suite("@ignoreBefore + @ignoreBetween — leading + internal, no trailing")(
        ok(beforeBetween, " 1 , 2"),
        ok(beforeBetween, "1,2"),
        no(beforeBetween, "1,2 "),
      ),
      suite("single-field product: @ignoreBetween is a no-op")(
        test("accept 1") { assert(SoloBetween.parser.parse(source("1")).map(_.n.value))(isRight(equalTo(BigInt(1)))) },
        test("reject ' 1' (no before)") { assert(SoloBetween.parser.parse(source(" 1")))(isLeft) },
        test("reject '1 ' (no after)") { assert(SoloBetween.parser.parse(source("1 ")))(isLeft) },
        test("SoloBefore accepts leading ws") { assert(SoloBefore.parser.parse(source("  1")).map(_.n.value))(isRight(equalTo(BigInt(1)))) },
        test("SoloBefore rejects trailing ws") { assert(SoloBefore.parser.parse(source("1 ")))(isLeft) },
        test("SoloAfter accepts trailing ws") { assert(SoloAfter.parser.parse(source("1  ")).map(_.n.value))(isRight(equalTo(BigInt(1)))) },
        test("SoloAfter rejects leading ws") { assert(SoloAfter.parser.parse(source(" 1")))(isLeft) },
      ),
      suite("whitespace that EXCLUDES newlines")(
        ok(noNl, " 1 , 2 "),   // spaces at gaps
        ok(noNl, "\t1,2"),      // leading tab
        no(noNl, "\n1,2"),      // leading newline
        no(noNl, "1,\n2"),      // internal newline
        no(noNl, "1,2\n"),      // trailing newline
      ),
      suite("single-terminal comment ignore (whitespace NOT ignorable)")(
        ok(comment, "1,2"),
        ok(comment, "/*a*/1,2"),   // leading comment
        ok(comment, "1/*a*/,2"),   // after x
        ok(comment, "1,/*a*/2"),   // after comma
        ok(comment, "1,2/*a*/"),   // trailing comment
        ok(comment, "/*a*/1/*b*/,/*c*/2/*d*/"),
        no(comment, " 1,2"),       // whitespace is NOT ignorable here
        no(comment, "1 ,2"),
      ),
      suite("FULL structural equality (spans included)")(
        parsesTo(BetweenPair.parser, "1 , 2") { s =>
          BetweenPair(Num("1", span(s, 0, 1), 1), `,`(",", span(s, 2, 3)), Num("2", span(s, 4, 5), 2))
        },
        parsesTo(BeforeAfterPair.parser, "  1,2  ") { s =>
          BeforeAfterPair(Num("1", span(s, 2, 3), 1), `,`(",", span(s, 3, 4)), Num("2", span(s, 4, 5), 2))
        },
        parsesTo(CommentPair.parser, "/*a*/1/*b*/,/*c*/2/*d*/") { s =>
          CommentPair(Num("1", spanOf(s, "1"), 1), `,`(",", spanOf(s, ",")), Num("2", spanOf(s, "2"), 2))
        },
      ),
    )

}
