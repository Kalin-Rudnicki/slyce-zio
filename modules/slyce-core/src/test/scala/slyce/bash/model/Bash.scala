package slyce.bash.model

import slyce.core.*
import slyce.core.builtIn.*
import slyce.parse.*

// =====| Separators / terminators |=====

/** Spaces / tabs between words (required before each arg after the first). */
@regex("[ \\t]+".r)
final case class Ws(text: String, span: Span.Range) extends Terminal

@regex(";".r)
final case class `;`(text: String, span: Span.Range) extends Terminal

@regex("\\r?\\n".r)
final case class Newline(text: String, span: Span.Range) extends Terminal

@regex("\"".r)
final case class `"`(text: String, span: Span.Range) extends Terminal

@regex("'".r)
final case class `'`(text: String, span: Span.Range) extends Terminal

// =====| BashCommand |=====

/** Single bash-like command for derive e2e: word (ws word)* [ws]? [ ';' | newline ]?
  *
  * Focus: unquoted / single-quoted / double-quoted words, spaces, trailing `;` or newline. No pipelines, redirects, or assignments yet.
  */
final case class BashCommand(
    head: Word,
    args: ElementList[SpacedWord],
    trailWs: ElementOption[Ws],
    end: ElementOption[CmdEnd],
) extends NonTerminal {
  override val span: Span.Range = {
    val afterHead =
      end.toOption
        .map(_.span)
        .orElse(trailWs.toOption.map(_.span))
        .orElse(args.headOption.map { _ =>
          args match {
            case n: NonEmptyElementList[?] => n.span
            case n: ElementNil             => n.span
          }
        })
        .getOrElse(head.span)
    head.span <> afterHead
  }
}
object BashCommand {
  val parser: Parser[BashCommand] = Parser.derived[BashCommand](2)
}

/** `ws` then another word — models required spacing between argv words. */
final case class SpacedWord(
    ws: Ws,
    word: Word,
) extends NonTerminal {
  override val span: Span.Range = ws.span <> word.span
}

sealed trait CmdEnd extends NonTerminal

final case class SemiEnd(semi: `;`) extends CmdEnd {
  override val span: Span.Range = semi.span
}

final case class NewlineEnd(nl: Newline) extends CmdEnd {
  override val span: Span.Range = nl.span
}

// =====| Words |=====

sealed trait Word extends Element { self: Terminal | NonTerminal => }

/** Unquoted argv token (no spaces, quotes, or `;`). */
@regex("""[^\s;'"\\]+""".r)
final case class Unquoted(text: String, span: Span.Range) extends Word, Terminal

final case class DoubleQuoted(
    open: `"`,
    body: ElementOption[DQuoteChars],
    close: `"`,
) extends Word,
      NonTerminal {
  override val span: Span.Range = open.span <> close.span
}

/** Non-empty contents of a double-quoted string (no `"`). Empty quotes use [[ElementOption.None]]. */
@regex("""[^"]+""".r)
final case class DQuoteChars(text: String, span: Span.Range) extends Terminal

final case class SingleQuoted(
    open: `'`,
    body: ElementOption[SQuoteChars],
    close: `'`,
) extends Word,
      NonTerminal {
  override val span: Span.Range = open.span <> close.span
}

/** Non-empty contents of a single-quoted string (no `'`). */
@regex("""[^']+""".r)
final case class SQuoteChars(text: String, span: Span.Range) extends Terminal
