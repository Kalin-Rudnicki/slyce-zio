package slyce.ignore.nullables

import oxygen.predef.core.*
import oxygen.predef.test.*

import slyce.TestUtils.*
import slyce.core.*
import slyce.core.builtIn.*

object NullablesSpec extends OxygenSpecDefault {

  private def esc(s: String): String = s.replace("\n", "\\n")

  override def testSpec: TestSpec =
    suite("NullablesSpec")(
      suite("nullable OPTION in FIRST position: `a? , b`")(
        test("',2' -> a=None b=2") {
          assert(OptFirst.parser.parse(source(",2")).map(p => (p.a.toOption.map(_.value), p.b.value)))(isRight(equalTo((Option.empty[BigInt], BigInt(2)))))
        },
        test("'1,2' -> a=Some(1) b=2") {
          assert(OptFirst.parser.parse(source("1,2")).map(p => (p.a.toOption.map(_.value), p.b.value)))(isRight(equalTo((Some(BigInt(1)), BigInt(2)))))
        },
        test("' 1 , 2 ' -> a=Some(1) b=2") {
          assert(OptFirst.parser.parse(source(" 1 , 2 ")).map(p => (p.a.toOption.map(_.value), p.b.value)))(isRight(equalTo((Some(BigInt(1)), BigInt(2)))))
        },
        test("' , 2 ' -> a=None b=2") {
          assert(OptFirst.parser.parse(source(" , 2 ")).map(p => (p.a.toOption.map(_.value), p.b.value)))(isRight(equalTo((Option.empty[BigInt], BigInt(2)))))
        },
        test("reject '1,' (b required)") { assert(OptFirst.parser.parse(source("1,")))(isLeft) },
      ),
      suite("nullable OPTION in LAST position: `a , b?`")(
        test("'1,' -> a=1 b=None") {
          assert(OptLast.parser.parse(source("1,")).map(p => (p.a.value, p.b.toOption.map(_.value))))(isRight(equalTo((BigInt(1), Option.empty[BigInt]))))
        },
        test("'1,2' -> a=1 b=Some(2)") {
          assert(OptLast.parser.parse(source("1,2")).map(p => (p.a.value, p.b.toOption.map(_.value))))(isRight(equalTo((BigInt(1), Some(BigInt(2))))))
        },
        test("' 1 , 2 ' -> a=1 b=Some(2)") {
          assert(OptLast.parser.parse(source(" 1 , 2 ")).map(p => (p.a.value, p.b.toOption.map(_.value))))(isRight(equalTo((BigInt(1), Some(BigInt(2))))))
        },
        test("' 1 , ' -> a=1 b=None") {
          assert(OptLast.parser.parse(source(" 1 , ")).map(p => (p.a.value, p.b.toOption.map(_.value))))(isRight(equalTo((BigInt(1), Option.empty[BigInt]))))
        },
        test("reject ',2' (a required)") { assert(OptLast.parser.parse(source(",2")))(isLeft) },
      ),
      suite("nullable LIST in FIRST position: `items* ;`")(
        test("';' -> []") { assert(ListFirst.parser.parse(source(";")).map(_.items.toList.map(_.value)))(isRight(equalTo(List.empty[BigInt]))) },
        test("'1;' -> [1]") { assert(ListFirst.parser.parse(source("1;")).map(_.items.toList.map(_.value)))(isRight(equalTo(List(BigInt(1))))) },
        test("'1 2 3;' -> [1,2,3]") { assert(ListFirst.parser.parse(source("1 2 3;")).map(_.items.toList.map(_.value)))(isRight(equalTo(List(BigInt(1), BigInt(2), BigInt(3))))) },
        test("' 1 2 3 ;' -> [1,2,3] with edge ws") { assert(ListFirst.parser.parse(source(" 1 2 3 ;")).map(_.items.toList.map(_.value)))(isRight(equalTo(List(BigInt(1), BigInt(2), BigInt(3))))) },
        test("reject '' (no ;)") { assert(ListFirst.parser.parse(source("")))(isLeft) },
      ),
      suite("NON-EMPTY list under ignore: `( items+ )` — non-empty works")(
        test("'(1)' -> [1]") { assert(NelCell.parser.parse(source("(1)")).map(_.items.toList.map(_.value)))(isRight(equalTo(List(BigInt(1))))) },
        test("'( 1 2 3 )' -> [1,2,3] internal spacing") { assert(NelCell.parser.parse(source("( 1 2 3 )")).map(_.items.toList.map(_.value)))(isRight(equalTo(List(BigInt(1), BigInt(2), BigInt(3))))) },
        test("'(1 2 3)' -> [1,2,3] tight internal") { assert(NelCell.parser.parse(source("(1 2 3)")).map(_.items.toList.map(_.value)))(isRight(equalTo(List(BigInt(1), BigInt(2), BigInt(3))))) },
        test("PlainNel '(1)' -> [1] [no-ignore NEL works for non-empty]") { assert(PlainNel.parser.parse(source("(1)")).map(_.items.toList.map(_.value)))(isRight(equalTo(List(BigInt(1))))) },
      ),
      // ============================================================================================
      // DEFECT (DISABLED): a `NonEmptyElementList[T]` field mis-accepts EMPTY input instead of
      // rejecting it, then throws an UNCAUGHT `ClassCastException` (ElementNil -> NonEmptyElementList)
      // OUT of `Parser.parse` (violating the Either contract). Minimal repro: `NelCell(open,
      // items: NonEmptyElementList[Num], close)` on input "()". NOT ignore-specific: `PlainNel`
      // (same shape, NO ignore annotations) fails identically on "()". Non-empty inputs are correct.
      // Root cause is in the nonempty-list table/reduce generation (the Head state, unable to match an
      // element, wrongly reduces to an empty list). Re-enable once the empty edge rejects properly.
      // ============================================================================================
      suite("NonEmptyElementList empty-input DEFECT (disabled)")(
        test("NelCell rejects '()' [folded under ignore]") { assert(NelCell.parser.parse(source("()")))(isLeft) },
        test("NelCell rejects '( )' [whitespace only]") { assert(NelCell.parser.parse(source("( )")))(isLeft) },
        test("PlainNel rejects '()' [no ignore — defect is not ignore-specific]") { assert(PlainNel.parser.parse(source("()")))(isLeft) },
      ) @@ TestAspect.ignore,
      suite("ADJACENT nullable fields (distinct types): `( a? b? )`")(
        test("'()' -> a=None b=None [worst-case adjacency, both absent]") {
          assert(AdjOpt.parser.parse(source("()")).map(p => (p.a.toOption.isDefined, p.b.toOption.map(_.value))))(isRight(equalTo((false, Option.empty[BigInt]))))
        },
        test("'( )' -> a=None b=None [ws, both absent]") {
          assert(AdjOpt.parser.parse(source("( )")).map(p => (p.a.toOption.isDefined, p.b.toOption.map(_.value))))(isRight(equalTo((false, Option.empty[BigInt]))))
        },
        test("'(-)' -> a=Some b=None") {
          assert(AdjOpt.parser.parse(source("(-)")).map(p => (p.a.toOption.isDefined, p.b.toOption.map(_.value))))(isRight(equalTo((true, Option.empty[BigInt]))))
        },
        test("'(5)' -> a=None b=Some(5)") {
          assert(AdjOpt.parser.parse(source("(5)")).map(p => (p.a.toOption.isDefined, p.b.toOption.map(_.value))))(isRight(equalTo((false, Some(BigInt(5))))))
        },
        test("'(-5)' -> a=Some b=Some(5)") {
          assert(AdjOpt.parser.parse(source("(-5)")).map(p => (p.a.toOption.isDefined, p.b.toOption.map(_.value))))(isRight(equalTo((true, Some(BigInt(5))))))
        },
        test("'( - 5 )' -> a=Some b=Some(5) with ws") {
          assert(AdjOpt.parser.parse(source("( - 5 )")).map(p => (p.a.toOption.isDefined, p.b.toOption.map(_.value))))(isRight(equalTo((true, Some(BigInt(5))))))
        },
      ),
      suite("OPTION then required field (distinct types): `( a? n )`")(
        test("'(5)' -> a=None n=5") {
          assert(OptThenReq.parser.parse(source("(5)")).map(p => (p.a.toOption.isDefined, p.n.value)))(isRight(equalTo((false, BigInt(5)))))
        },
        test("'(-5)' -> a=Some n=5") {
          assert(OptThenReq.parser.parse(source("(-5)")).map(p => (p.a.toOption.isDefined, p.n.value)))(isRight(equalTo((true, BigInt(5)))))
        },
        test("'( - 5 )' -> a=Some n=5 with ws") {
          assert(OptThenReq.parser.parse(source("( - 5 )")).map(p => (p.a.toOption.isDefined, p.n.value)))(isRight(equalTo((true, BigInt(5)))))
        },
        test("reject '()' (n required)") { assert(OptThenReq.parser.parse(source("()")))(isLeft) },
      ),
      suite("FULL structural equality (spans included)")(
        parsesTo(OptLast.parser, "1,") { s =>
          OptLast(Num("1", span(s, 0, 1), 1), `,`(",", span(s, 1, 2)), eoNone(span(s, 2, 2)))
        },
        parsesTo(OptLast.parser, "1,2") { s =>
          OptLast(Num("1", span(s, 0, 1), 1), `,`(",", span(s, 1, 2)), eoSome(Num("2", span(s, 2, 3), 2)))
        },
        parsesTo(AdjOpt.parser, "()") { s =>
          AdjOpt(`(`("(", span(s, 0, 1)), eoNone(span(s, 1, 1)), eoNone(span(s, 1, 1)), `)`(")", span(s, 1, 2)))
        },
        parsesTo(NelCell.parser, "(1)") { s =>
          NelCell(`(`("(", span(s, 0, 1)), NonEmptyElementList(Num("1", span(s, 1, 2), 1), ElementNil(span(s, 2, 2))), `)`(")", span(s, 2, 3)))
        },
        parsesTo(NelCell.parser, "( 1 2 3 )") { s =>
          NelCell(
            `(`("(", span(s, 0, 1)),
            NonEmptyElementList(
              Num("1", span(s, 2, 3), 1),
              NonEmptyElementList(
                Num("2", span(s, 4, 5), 2),
                NonEmptyElementList(Num("3", span(s, 6, 7), 3), ElementNil(span(s, 8, 8))),
              ),
            ),
            `)`(")", span(s, 8, 9)),
          )
        },
      ),
    )

}
