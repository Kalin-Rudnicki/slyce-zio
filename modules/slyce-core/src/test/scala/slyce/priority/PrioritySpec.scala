package slyce.priority

import oxygen.predef.test.*
import zio.test.typeCheck

import slyce.TestUtils.*
import slyce.parse.Parser
import slyce.priority.model.*

/** Terminal PRIORITY: an opt-in, deterministic tie-break for EQUAL-LENGTH lexer matches.
  *
  * Maximal munch (longest-match) is always primary; priority only decides same-length ties. A higher-priority keyword terminal (`@priority(1) @regex("struct")`) therefore beats the default-priority
  * `Ident` regex on the exact string `"struct"`, while `structure` still lexes as one identifier.
  */
object PrioritySpec extends OxygenSpecDefault {

  private def one(input: String)(using Trace, SourceLocation): Either[Any, Tok] =
    One.parser.parse(source(input)).map(_.tok)

  override def testSpec: TestSpec =
    suite("PrioritySpec")(
      suite("equal-length tie — priority wins")(
        test("\"struct\" lexes as KwStruct (not Ident)") {
          assert(one("struct"))(isRight(isSubtype[KwStruct](hasField("text", _.text, equalTo("struct")))))
        },
      ),
      suite("maximal munch preserved — longest match beats priority")(
        test("\"structure\" lexes as ONE Ident") {
          assert(one("structure"))(isRight(isSubtype[Ident](hasField("text", _.text, equalTo("structure")))))
        },
        test("\"structs\" lexes as ONE Ident (keyword is a strict prefix)") {
          assert(one("structs"))(isRight(isSubtype[Ident](hasField("text", _.text, equalTo("structs")))))
        },
        test("\"struct_\" lexes as ONE Ident") {
          assert(one("struct_"))(isRight(isSubtype[Ident](hasField("text", _.text, equalTo("struct_")))))
        },
      ),
      suite("no overlap — ordinary identifiers unaffected")(
        test("\"other\" lexes as Ident") {
          assert(one("other"))(isRight(isSubtype[Ident](hasField("text", _.text, equalTo("other")))))
        },
      ),
      suite("equal-length tie-break resolves the SAME at every position")(
        // These vary WHICH token sits at each position; the equal-length outcome (keyword-vs-identifier)
        // is asserted DIRECTLY at each slot, so each case is load-bearing rather than a repeat.
        test("\"struct structure\" -> (KwStruct, Ident \"structure\")") {
          assert(Pair.parser.parse(source("struct structure")))(
            isRight(
              hasField[Pair, Tok]("a", _.a, isSubtype[KwStruct](anything)) &&
                hasField[Pair, Tok]("b", _.b, isSubtype[Ident](hasField("text", _.text, equalTo("structure")))),
            ),
          )
        },
        test("\"structure struct\" -> (Ident \"structure\", KwStruct)") {
          assert(Pair.parser.parse(source("structure struct")))(
            isRight(
              hasField[Pair, Tok]("a", _.a, isSubtype[Ident](hasField("text", _.text, equalTo("structure")))) &&
                hasField[Pair, Tok]("b", _.b, isSubtype[KwStruct](anything)),
            ),
          )
        },
        test("\"struct struct\" -> (KwStruct, KwStruct)") {
          assert(Pair.parser.parse(source("struct struct")))(
            isRight(
              hasField[Pair, Tok]("a", _.a, isSubtype[KwStruct](anything)) &&
                hasField[Pair, Tok]("b", _.b, isSubtype[KwStruct](anything)),
            ),
          )
        },
      ),
      suite("grammar validity — prefix overlap compiles, equal-length overlap hard-fails")(
        // BACK-COMPAT (M1): sibling prefix-literal terminals (`in` / `insert`, same default priority) are
        // resolved by maximal munch and are NOT a lexer ambiguity. The mere fact that `One2.parser` derives
        // and both strings parse proves `GrammarValidity` no longer over-rejects such prefix overlaps.
        test("prefix-literal siblings (\"in\" / \"insert\") derive without a priority and lex by munch") {
          assert(One2.parser.parse(source("insert")).map(_.tok))(isRight(isSubtype[KwInsert](hasField("text", _.text, equalTo("insert"))))) &&
          assert(One2.parser.parse(source("in")).map(_.tok))(isRight(isSubtype[KwIn](hasField("text", _.text, equalTo("in")))))
        },
        // A GENUINE equal-length, equal-priority overlap (`struct` vs an identifier regex, both default
        // priority) must still hard-fail derivation. `BadOne` compiles as a type; deriving a parser for it
        // aborts the macro, which `typeCheck` captures as a Left.
        test("equal-length, equal-priority overlap still hard-fails derivation") {
          assertZIO(typeCheck("Parser.derived[BadOne](1)"))(isLeft(anything))
        },
      ),
    )

}
