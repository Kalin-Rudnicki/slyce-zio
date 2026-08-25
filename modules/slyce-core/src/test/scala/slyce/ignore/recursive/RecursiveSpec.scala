package slyce.ignore.recursive

import oxygen.predef.core.*
import oxygen.predef.test.*

import slyce.TestUtils.*

/** Regression: recursive operator-expression grammars with optional ignore slots must build at small `k` and parse with arbitrary whitespace/comments interleaved. Pre-fix, `Doc.parser` /
  * `Decl.parser` failed to build at all (the look-ahead never converged); post-fix they build at k=2.
  */
object RecursiveSpec extends OxygenSpecDefault {

  private def parsesExpr(input: String, nums: List[Int], ops: List[String])(using Trace, SourceLocation): TestSpec =
    test(input.replace("\n", "\\n")) {
      val got = Doc.parser.parse(source(input)).map(d => (Doc.nums(d.expr), Doc.ops(d.expr)))
      assert(got)(isRight(equalTo((nums.map(BigInt(_)), ops))))
    }

  override def testSpec: TestSpec =
    suite("RecursiveSpec")(
      suite("recursive infix expression, ignore around the operator")(
        parsesExpr("1", List(1), Nil),
        parsesExpr("1 + 2 * 3", List(1, 2, 3), List("+", "*")),
        parsesExpr("1+2*3", List(1, 2, 3), List("+", "*")), // no whitespace
        parsesExpr("( 1 +2 )* 3", List(1, 2, 3), List("+", "*")),
        parsesExpr("(1+2)*3", List(1, 2, 3), List("+", "*")),
        parsesExpr("a + b", Nil, List("a", "+", "b")),
        parsesExpr("1 /* c */ + /* d */ 2", List(1, 2), List("+")), // block comments as noise
        parsesExpr("1 + // trailing\n 2", List(1, 2), List("+")), // line comment as noise
        parsesExpr("  1 + 2  ", List(1, 2), List("+")), // leading/trailing noise
        test("reject: 1 +") {
          assert(Doc.parser.parse(source("1 +")))(isLeft)
        },
        test("reject: 1 2 (missing operator)") {
          assert(Doc.parser.parse(source("1 2")))(isLeft)
        },
      ),
      suite("soft-keyword fork past an ignore slot")(
        test("import foo -> Import") {
          val got = Decl.parser.parse(source("import foo"))
          assert(got.map { case i: Import => (i.kw.text, i.name.text); case _ => ("", "") })(isRight(equalTo(("import", "foo"))))
        },
        test("import/* c */foo -> Import (noise between)") {
          val got = Decl.parser.parse(source("import/* c */foo"))
          assert(got.map { case i: Import => (i.kw.text, i.name.text); case _ => ("", "") })(isRight(equalTo(("import", "foo"))))
        },
        test("x : Int -> Def") {
          val got = Decl.parser.parse(source("x : Int"))
          assert(got.map { case d: Def => (d.name.text, d.tpe.text); case _ => ("", "") })(isRight(equalTo(("x", "Int"))))
        },
        test("x/* c */:Int -> Def (noise before colon)") {
          val got = Decl.parser.parse(source("x/* c */:Int"))
          assert(got.map { case d: Def => (d.name.text, d.tpe.text); case _ => ("", "") })(isRight(equalTo(("x", "Int"))))
        },
      ),
    )

}
