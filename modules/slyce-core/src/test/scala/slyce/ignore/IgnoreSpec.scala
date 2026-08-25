package slyce.ignore

import oxygen.predef.test.*

import slyce.TestUtils.*
import slyce.ignore.model.*

object IgnoreSpec extends OxygenSpecDefault {

  /** Parse succeeds and yields the two numbers, regardless of surrounding/internal whitespace. */
  private def parsesToPoint(input: String, x: BigInt, y: BigInt)(using Trace, SourceLocation): TestSpec =
    test(input.replace("\n", "\\n")) {
      val s = source(input)
      val got = Point.parser.parse(s).map(p => (p.x.value, p.y.value))
      assert(got)(isRight(equalTo((x, y))))
    }

  private def failsToParse(input: String)(using Trace, SourceLocation): TestSpec =
    test(s"reject: ${input.replace("\n", "\\n")}") {
      assert(Point.parser.parse(source(input)))(isLeft)
    }

  private def parsesToCPoint(input: String, x: BigInt, y: BigInt)(using Trace, SourceLocation): TestSpec =
    test(input.replace("\n", "\\n")) {
      val s = source(input)
      val got = CPoint.parser.parse(s).map(p => (p.x.value, p.y.value))
      assert(got)(isRight(equalTo((x, y))))
    }

  override def testSpec: TestSpec =
    suite("IgnoreSpec")(
      suite("whitespace is ignored around and between structure")(
        parsesToPoint("(1,2)", 1, 2),           // no whitespace
        parsesToPoint("( 1 , 2 )", 1, 2),        // spaces at every gap
        parsesToPoint("  (1,2)  ", 1, 2),        // leading/trailing (before/after)
        parsesToPoint("(\n  1,\n  2\n)", 1, 2),  // newlines + indentation
        parsesToPoint("(1 ,2)", 1, 2),           // asymmetric internal spacing
        parsesToPoint("( 10 , 20 )", 10, 20),    // multi-digit
      ),
      suite("sum-terminal ignore: whitespace + comments interleaved")(
        parsesToCPoint("(1,2)", 1, 2),                          // still works with no noise
        parsesToCPoint("( 1 , 2 )", 1, 2),                      // whitespace
        parsesToCPoint("(/* a */1,2)", 1, 2),                   // block comment before a field
        parsesToCPoint("( 1 /* mid */ , /* */ 2 )", 1, 2),      // interleaved ws + block comments
        parsesToCPoint("(1, // trailing\n 2)", 1, 2),           // line comment ended by newline
        parsesToCPoint("  // lead\n (1,2) // tail", 1, 2),      // comments before/after the whole thing
      ),
      suite("nullable OPTION field under ignore (review #1)")(
        test("(1 2) -> a=1 b=Some(2)") {
          assert(OptCell.parser.parse(source("(1 2)")).map(c => (c.a.value, c.b.toOption.map(_.value))))(
            isRight(equalTo((BigInt(1), Some(BigInt(2))))),
          )
        },
        test("(1) -> a=1 b=None [absent nullable, no adjacent ignore]") {
          assert(OptCell.parser.parse(source("(1)")).map(c => (c.a.value, c.b.toOption.map(_.value))))(
            isRight(equalTo((BigInt(1), Option.empty[BigInt]))),
          )
        },
        test("( 1 ) -> a=1 b=None with surrounding whitespace") {
          assert(OptCell.parser.parse(source("( 1 )")).map(c => (c.a.value, c.b.toOption.map(_.value))))(
            isRight(equalTo((BigInt(1), Option.empty[BigInt]))),
          )
        },
        test("( 1 2 ) -> a=1 b=Some(2) with whitespace at every gap") {
          assert(OptCell.parser.parse(source("( 1 2 )")).map(c => (c.a.value, c.b.toOption.map(_.value))))(
            isRight(equalTo((BigInt(1), Some(BigInt(2))))),
          )
        },
      ),
      suite("nullable LIST field under ignore (review #1 + list-internal spacing)")(
        test("() -> empty") {
          assert(ListCell.parser.parse(source("()")).map(_.items.toList.map(_.value)))(isRight(equalTo(List.empty[BigInt])))
        },
        test("( ) -> empty [whitespace, empty list, no adjacent ignore]") {
          assert(ListCell.parser.parse(source("( )")).map(_.items.toList.map(_.value)))(isRight(equalTo(List.empty[BigInt])))
        },
        test("(1) -> [1]") {
          assert(ListCell.parser.parse(source("(1)")).map(_.items.toList.map(_.value)))(isRight(equalTo(List(BigInt(1)))))
        },
        test("( 1 2 3 ) -> [1,2,3] list-internal spacing") {
          assert(ListCell.parser.parse(source("( 1 2 3 )")).map(_.items.toList.map(_.value)))(
            isRight(equalTo(List(BigInt(1), BigInt(2), BigInt(3)))),
          )
        },
      ),
      suite("sum-child product with ignore (review #2)")(
        test("( 7 ) via Wrapped sum root -> Boxed(7)") {
          val got = Wrapped.parser.parse(source("( 7 )")).map { case b: Boxed => b.n.value }
          assert(got)(isRight(equalTo(BigInt(7))))
        },
      ),
      suite("required tokens still required")(
        failsToParse("(1 2)"),   // missing comma (Point)
        failsToParse("1,2"),     // missing parens (Point)
      ),
    )

}
