package slyce.json

import oxygen.predef.test.*

import slyce.TestUtils.*
import slyce.core.*
import slyce.core.builtIn.*
import slyce.json.model.*

object JsonSpec extends OxygenSpecDefault {

  private def str(s: Source, content: String, openAt: Int): Str = {
    val open = `"`("\"", span(s, openAt, openAt + 1))
    val closeAt = openAt + 1 + content.length
    val parts =
      if content.isEmpty then elementList[StrPart](span(s, openAt + 1, openAt + 1))()
      else elementList[StrPart](span(s, closeAt, closeAt))(Chars(content, span(s, openAt + 1, closeAt)))
    val close = `"`("\"", span(s, closeAt, closeAt + 1))
    Str(open, parts, close)
  }

  override def testSpec: TestSpec =
    (
    suite("JsonSpec")(
      suite("valid")(
        parsesTo(Json.parser, "null") { s =>
          NullLit("null", spanOf(s, "null"))
        },
        parsesTo(Json.parser, "true") { s =>
          TrueLit("true", spanOf(s, "true"))
        },
        parsesTo(Json.parser, "false") { s =>
          FalseLit("false", spanOf(s, "false"))
        },
        parsesTo(Json.parser, "0") { s =>
          IntLit("0", spanOf(s, "0"), 0)
        },
        parsesTo(Json.parser, "42") { s =>
          IntLit("42", spanOf(s, "42"), 42)
        },
        parsesTo(Json.parser, "-7") { s =>
          IntLit("-7", spanOf(s, "-7"), -7)
        },
        parsesTo(Json.parser, "3.14") { s =>
          FloatLit("3.14", spanOf(s, "3.14"), BigDecimal("3.14"))
        },
        parsesTo(Json.parser, "\"\"") { s =>
          str(s, "", 0)
        },
        parsesTo(Json.parser, "\"hello\"") { s =>
          str(s, "hello", 0)
        },
        parsesTo(Json.parser, "\"a\\\"b\"") { s =>
          Str(
            `"`("\"", span(s, 0, 1)),
            elementList[StrPart](span(s, 5, 5))(
              Chars("a", span(s, 1, 2)),
              EscChar("\\\"", span(s, 2, 4), '"'),
              Chars("b", span(s, 4, 5)),
            ),
            `"`("\"", span(s, 5, 6)),
          )
        },
        parsesTo(Json.parser, "[]") { s =>
          Arr(
            `[`("[", spanOf(s, "[")),
            eoNone(span(s, 1, 1)),
            `]`("]", spanOf(s, "]")),
          )
        },
        parsesTo(Json.parser, "[1]") { s =>
          Arr(
            `[`("[", span(s, 0, 1)),
            eoSome(
              ArrBody(
                IntLit("1", span(s, 1, 2), 1),
                elementList[CommaJson](span(s, 2, 2))(),
              ),
            ),
            `]`("]", span(s, 2, 3)),
          )
        },
        parsesTo(Json.parser, "[1,2]") { s =>
          Arr(
            `[`("[", span(s, 0, 1)),
            eoSome(
              ArrBody(
                IntLit("1", span(s, 1, 2), 1),
                elementList[CommaJson](span(s, 4, 4))(
                  CommaJson(`,`( ",", span(s, 2, 3)), IntLit("2", span(s, 3, 4), 2)),
                ),
              ),
            ),
            `]`("]", span(s, 4, 5)),
          )
        },
        parsesTo(Json.parser, "[null,true,[]]") { s =>
          Arr(
            `[`("[", span(s, 0, 1)),
            eoSome(
              ArrBody(
                NullLit("null", span(s, 1, 5)),
                elementList[CommaJson](span(s, 13, 13))(
                  CommaJson(`,`( ",", span(s, 5, 6)), TrueLit("true", span(s, 6, 10))),
                  CommaJson(
                    `,`( ",", span(s, 10, 11)),
                    Arr(
                      `[`("[", span(s, 11, 12)),
                      eoNone(span(s, 12, 12)),
                      `]`("]", span(s, 12, 13)),
                    ),
                  ),
                ),
              ),
            ),
            `]`("]", span(s, 13, 14)),
          )
        },
        parsesTo(Json.parser, "{}") { s =>
          Obj(
            `{`("{", spanOf(s, "{")),
            eoNone(span(s, 1, 1)),
            `}`("}", spanOf(s, "}")),
          )
        },
        parsesTo(Json.parser, "{\"a\":1}") { s =>
          Obj(
            `{`("{", span(s, 0, 1)),
            eoSome(
              ObjBody(
                KeyPair(
                  str(s, "a", 1),
                  `:`(":", span(s, 4, 5)),
                  IntLit("1", span(s, 5, 6), 1),
                ),
                elementList[CommaKeyPair](span(s, 6, 6))(),
              ),
            ),
            `}`("}", span(s, 6, 7)),
          )
        },
        parsesTo(Json.parser, "{\"a\":1,\"b\":false}") { s =>
          Obj(
            `{`("{", span(s, 0, 1)),
            eoSome(
              ObjBody(
                KeyPair(
                  str(s, "a", 1),
                  `:`(":", span(s, 4, 5)),
                  IntLit("1", span(s, 5, 6), 1),
                ),
                elementList[CommaKeyPair](span(s, 16, 16))(
                  CommaKeyPair(
                    `,`( ",", span(s, 6, 7)),
                    KeyPair(
                      str(s, "b", 7),
                      `:`(":", span(s, 10, 11)),
                      FalseLit("false", span(s, 11, 16)),
                    ),
                  ),
                ),
              ),
            ),
            `}`("}", span(s, 16, 17)),
          )
        },
      ),
      suite("invalid")(
        failsToParse(Json.parser, ""),
        failsToParse(Json.parser, "nul"),
        failsToParse(Json.parser, "True"),
        failsToParse(Json.parser, "01"),
        failsToParse(Json.parser, "1."),
        failsToParse(Json.parser, "\""),
        failsToParse(Json.parser, "\"unterminated"),
        failsToParse(Json.parser, "["),
        failsToParse(Json.parser, "[1,]"),
        failsToParse(Json.parser, "[1 2]"),
        failsToParse(Json.parser, "{"),
        failsToParse(Json.parser, "{1:2}"),
        failsToParse(Json.parser, "{\"a\"}"),
        failsToParse(Json.parser, "{\"a\":}"),
        failsToParse(Json.parser, "null true"),
      ),
    )) @@ TestAspect.ignore // calculator e2e isolation


}
