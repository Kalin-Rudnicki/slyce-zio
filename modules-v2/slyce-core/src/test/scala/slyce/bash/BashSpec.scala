package slyce.bash

import oxygen.predef.test.*

import slyce.TestUtils.*
import slyce.bash.model.*
import slyce.core.*
import slyce.core.builtIn.*

object BashSpec extends OxygenSpecDefault {

  private def uq(s: Source, text: String, from: Int = 0): Unquoted =
    Unquoted(text, spanOf(s, text, from))

  private def wsAt(s: Source, start: Int, end: Int): Ws =
    Ws(s.text.substring(start, end), span(s, start, end))

  /** First run of spaces/tabs at or after `from`. */
  private def nextWs(s: Source, from: Int): (Int, Int) = {
    val i = s.text.indexWhere(c => c == ' ' || c == '\t', from)
    require(i >= 0, s"no ws from $from in ${s.text.unesc}")
    var j = i
    while j < s.text.length && (s.text(j) == ' ' || s.text(j) == '\t') do j += 1
    (i, j)
  }

  private def spaced(s: Source, word: Word, wsFrom: Int): SpacedWord = {
    val (a, b) = nextWs(s, wsFrom)
    SpacedWord(wsAt(s, a, b), word)
  }

  private def dq(s: Source, openAt: Int, closeAt: Int): DoubleQuoted = {
    val body =
      if closeAt == openAt + 1 then eoNone(span(s, closeAt, closeAt))
      else eoSome(DQuoteChars(s.text.substring(openAt + 1, closeAt), span(s, openAt + 1, closeAt)))
    DoubleQuoted(
      `"`("\"", span(s, openAt, openAt + 1)),
      body,
      `"`("\"", span(s, closeAt, closeAt + 1)),
    )
  }

  private def sq(s: Source, openAt: Int, closeAt: Int): SingleQuoted = {
    val body =
      if closeAt == openAt + 1 then eoNone(span(s, closeAt, closeAt))
      else eoSome(SQuoteChars(s.text.substring(openAt + 1, closeAt), span(s, openAt + 1, closeAt)))
    SingleQuoted(
      `'`("'", span(s, openAt, openAt + 1)),
      body,
      `'`("'", span(s, closeAt, closeAt + 1)),
    )
  }

  private def cmd(
      head: Word,
      args: ElementList[SpacedWord],
      trailWs: ElementOption[Ws],
      end: ElementOption[CmdEnd],
  ): BashCommand =
    BashCommand(head, args, trailWs, end)

  override def testSpec: TestSpec =
    suite("BashSpec")(
      suite("valid")(
        parsesTo(BashCommand.parser, "echo") { s =>
          cmd(uq(s, "echo"), elementList[SpacedWord](eofSpan(s))(), eoNone(eofSpan(s)), eoNone(eofSpan(s)))
        },
        parsesTo(BashCommand.parser, "echo hello") { s =>
          cmd(
            uq(s, "echo"),
            elementList[SpacedWord](eofSpan(s))(spaced(s, uq(s, "hello"), 0)),
            eoNone(eofSpan(s)),
            eoNone(eofSpan(s)),
          )
        },
        parsesTo(BashCommand.parser, "echo hello world") { s =>
          val hello = s.text.indexOf("hello")
          cmd(
            uq(s, "echo"),
            elementList[SpacedWord](eofSpan(s))(
              spaced(s, uq(s, "hello"), 0),
              spaced(s, uq(s, "world"), hello),
            ),
            eoNone(eofSpan(s)),
            eoNone(eofSpan(s)),
          )
        },
        parsesTo(BashCommand.parser, "echo  hello") { s =>
          cmd(
            uq(s, "echo"),
            elementList[SpacedWord](eofSpan(s))(spaced(s, uq(s, "hello"), 0)),
            eoNone(eofSpan(s)),
            eoNone(eofSpan(s)),
          )
        },
        parsesTo(BashCommand.parser, "echo;") { s =>
          val semi = s.text.indexOf(';')
          cmd(
            uq(s, "echo"),
            elementList[SpacedWord](span(s, semi, semi))(),
            eoNone(span(s, semi, semi)),
            eoSome(SemiEnd(`;`( ";", span(s, semi, semi + 1)))),
          )
        },
        parsesTo(BashCommand.parser, "echo ;") { s =>
          val (a, b) = nextWs(s, 0)
          cmd(
            uq(s, "echo"),
            elementList[SpacedWord](span(s, a, a))(),
            eoSome(wsAt(s, a, b)),
            eoSome(SemiEnd(`;`( ";", spanOf(s, ";")))),
          )
        },
        parsesTo(BashCommand.parser, "echo\n") { s =>
          val nl = s.text.indexOf('\n')
          cmd(
            uq(s, "echo"),
            elementList[SpacedWord](span(s, nl, nl))(),
            eoNone(span(s, nl, nl)),
            eoSome(NewlineEnd(Newline("\n", span(s, nl, nl + 1)))),
          )
        },
        parsesTo(BashCommand.parser, "echo hello;") { s =>
          val semi = s.text.indexOf(';')
          cmd(
            uq(s, "echo"),
            elementList[SpacedWord](span(s, semi, semi))(spaced(s, uq(s, "hello"), 0)),
            eoNone(span(s, semi, semi)),
            eoSome(SemiEnd(`;`( ";", span(s, semi, semi + 1)))),
          )
        },
        parsesTo(BashCommand.parser, "echo \"hi\"") { s =>
          val q0 = s.text.indexOf('"')
          val q1 = s.text.lastIndexOf('"')
          cmd(
            uq(s, "echo"),
            elementList[SpacedWord](eofSpan(s))(spaced(s, dq(s, q0, q1), 0)),
            eoNone(eofSpan(s)),
            eoNone(eofSpan(s)),
          )
        },
        parsesTo(BashCommand.parser, "echo 'hi'") { s =>
          val q0 = s.text.indexOf('\'')
          val q1 = s.text.lastIndexOf('\'')
          cmd(
            uq(s, "echo"),
            elementList[SpacedWord](eofSpan(s))(spaced(s, sq(s, q0, q1), 0)),
            eoNone(eofSpan(s)),
            eoNone(eofSpan(s)),
          )
        },
        parsesTo(BashCommand.parser, "echo \"\"") { s =>
          val q0 = s.text.indexOf('"')
          val q1 = s.text.lastIndexOf('"')
          cmd(
            uq(s, "echo"),
            elementList[SpacedWord](eofSpan(s))(spaced(s, dq(s, q0, q1), 0)),
            eoNone(eofSpan(s)),
            eoNone(eofSpan(s)),
          )
        },
        parsesTo(BashCommand.parser, "echo 'a b'") { s =>
          val q0 = s.text.indexOf('\'')
          val q1 = s.text.lastIndexOf('\'')
          cmd(
            uq(s, "echo"),
            elementList[SpacedWord](eofSpan(s))(spaced(s, sq(s, q0, q1), 0)),
            eoNone(eofSpan(s)),
            eoNone(eofSpan(s)),
          )
        },
        parsesTo(BashCommand.parser, "\"cmd\" 'arg' bare") { s =>
          val dq0 = s.text.indexOf('"')
          val dq1 = s.text.indexOf('"', dq0 + 1)
          val sq0 = s.text.indexOf('\'')
          val sq1 = s.text.indexOf('\'', sq0 + 1)
          cmd(
            dq(s, dq0, dq1),
            elementList[SpacedWord](eofSpan(s))(
              spaced(s, sq(s, sq0, sq1), dq1),
              spaced(s, uq(s, "bare"), sq1),
            ),
            eoNone(eofSpan(s)),
            eoNone(eofSpan(s)),
          )
        },
      ),
      suite("invalid")(
        failsToParse(BashCommand.parser, ""),
        failsToParse(BashCommand.parser, "   "),
        failsToParse(BashCommand.parser, ";"),
        failsToParse(BashCommand.parser, "echo \""),
        failsToParse(BashCommand.parser, "echo '"),
        failsToParse(BashCommand.parser, "echo a;echo b"),
        failsToParse(BashCommand.parser, "echo\nfoo"),
      ),
    )

}
