package slyce.ignore.sinks

import oxygen.predef.core.*
import oxygen.predef.test.*

import slyce.TestUtils.*
import slyce.core.*
import slyce.core.builtIn.*

object SinkSpec extends OxygenSpecDefault {

  private def esc(s: String): String = s.replace("\n", "\\n")

  // ---- Sink 1 : render a parsed JVal back to canonical (noise-free) text ----
  private def render(v: JVal): String = v match {
    case n: JNum => n.value.toString
    case a: JArr => "[" + a.values.map(render).mkString(",") + "]"
    case o: JObj => "{" + o.pairs.map(p => s"${p.key.text}:${render(p.value)}").mkString(",") + "}"
  }
  private def json(input: String, canonical: String)(using Trace, SourceLocation): TestSpec =
    test(s"${esc(input)} -> $canonical") {
      assert(JDoc.parser.parse(source(input)).map(d => render(d.value)))(isRight(equalTo(canonical)))
    }
  private def jsonNo(input: String)(using Trace, SourceLocation): TestSpec =
    test(s"reject ${esc(input)}") { assert(JDoc.parser.parse(source(input)))(isLeft) }

  // ---- Sink 2 : project Func ----
  private def func(input: String, name: String, params: List[String], stmts: List[String])(using Trace, SourceLocation): TestSpec =
    test(s"${esc(input)}") {
      val got = Func.parser.parse(source(input)).map(f => (f.name.text, f.params.map(_.text), f.body.stmts.toList.map(_.name.text)))
      assert(got)(isRight(equalTo((name, params, stmts))))
    }
  private def funcNo(input: String)(using Trace, SourceLocation): TestSpec =
    test(s"reject ${esc(input)}") { assert(Func.parser.parse(source(input)))(isLeft) }

  // ---- Sink 3 : project RecordList ----
  private def recs(input: String, expected: List[(String, Option[BigInt])])(using Trace, SourceLocation): TestSpec =
    test(s"${esc(input)}") {
      val got = RecordList.parser.parse(source(input)).map(_.records.map(r => (r.key.text, r.value.toOption.map(_.value))))
      assert(got)(isRight(equalTo(expected)))
    }
  private def recsNo(input: String)(using Trace, SourceLocation): TestSpec =
    test(s"reject ${esc(input)}") { assert(RecordList.parser.parse(source(input)))(isLeft) }

  override def testSpec: TestSpec =
    suite("SinkSpec")(
      suite("Sink 1 — JSON-ish, whitespace + comments ignored everywhere")(
        json("5", "5"),
        json("  5  ", "5"),
        json("/* a */ 5 // b", "5"),
        json("[]", "[]"),
        json("[ ]", "[]"),
        json("[1,2,3]", "[1,2,3]"),
        json("[ 1 , 2 , 3 ]", "[1,2,3]"),
        json("[1, /* c */ 2, // line\n 3]", "[1,2,3]"),
        json("{}", "{}"),
        json("{ }", "{}"),
        json("{a:1}", "{a:1}"),
        json("{ a : 1 , b : 2 }", "{a:1,b:2}"),
        json("[{a:1},{b:[2,3]}]", "[{a:1},{b:[2,3]}]"),
        json("  // header\n [ 1 , [ 2 , 3 ] , { x : 4 } ] // trailing\n", "[1,[2,3],{x:4}]"),
        json("{ outer : { inner : [ 1 , 2 ] } }", "{outer:{inner:[1,2]}}"),
        jsonNo(""),
        jsonNo("[1 2]"),   // missing comma
        jsonNo("[1,]"),    // trailing comma unsupported
        jsonNo("[1,,2]"),  // double comma
        jsonNo("{a 1}"),   // missing colon
        jsonNo("{a:}"),    // missing value
        jsonNo("5 6"),     // trailing token
      ),
      suite("Sink 2 — function language with comments")(
        func("f(){}", "f", Nil, Nil),
        func("f ( ) { }", "f", Nil, Nil),
        func("f(a){b;}", "f", List("a"), List("b")),
        func("add(a, b, c) { x; y; }", "add", List("a", "b", "c"), List("x", "y")),
        func("/* doc */ main ( ) { run ; } // end", "main", Nil, List("run")),
        func("g(\n  a,\n  b\n) {\n  s1;\n  s2;\n}", "g", List("a", "b"), List("s1", "s2")),
        funcNo("f(a b){}"),   // missing comma
        funcNo("f() {"),      // unclosed block
        funcNo("f(){x}"),     // missing semicolon
        funcNo("f(,){}"),     // leading comma
        funcNo("(){}"),       // missing name
      ),
      suite("Sink 3 — comma-separated records with optional field")(
        recs("{a:1}", List(("a", Some(BigInt(1))))),
        recs("{a:}", List(("a", None))),
        recs("{a:1},{b:2}", List(("a", Some(BigInt(1))), ("b", Some(BigInt(2))))),
        recs("{ a : 1 } , { b : } , { c : 3 }", List(("a", Some(BigInt(1))), ("b", None), ("c", Some(BigInt(3))))),
        recs("/* r */ { a : 1 } // c", List(("a", Some(BigInt(1))))),
        recsNo(""),
        recsNo("{a:1},"),     // trailing comma (CommaRecord needs a record)
        recsNo("{a:1} {b:2}"), // missing comma
        recsNo("{a 1}"),      // missing colon
      ),
      suite("FULL structural equality (spans included)")(
        parsesTo(JDoc.parser, "[ 1 , 2 ]") { s =>
          JDoc(
            JArr(
              `[`("[", span(s, 0, 1)),
              eoSome(JNum("1", span(s, 2, 3), 1)),
              elementList[JComma](span(s, 8, 8))(
                JComma(`,`(",", span(s, 4, 5)), JNum("2", span(s, 6, 7), 2)),
              ),
              `]`("]", span(s, 8, 9)),
            ),
          )
        },
        parsesTo(RecordList.parser, "{a:1}") { s =>
          RecordList(
            Record(
              `{`("{", span(s, 0, 1)),
              Id("a", span(s, 1, 2)),
              `:`(":", span(s, 2, 3)),
              eoSome(JNum("1", span(s, 3, 4), 1)),
              `}`("}", span(s, 4, 5)),
            ),
            elementList[CommaRecord](span(s, 5, 5))(),
          )
        },
      ),
    )

}
