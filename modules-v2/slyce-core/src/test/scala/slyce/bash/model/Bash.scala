package slyce.bash.model

import slyce.core.*
import slyce.core.builtIn.*
import slyce.parse.*

// =====| Punctuation |=====

@regex("\\|".r) final case class Pipe(text: String, span: Span.Range) extends Terminal
@regex(";".r) final case class `;`(text: String, span: Span.Range) extends Terminal
@regex("=".r) final case class `=`(text: String, span: Span.Range) extends Terminal
@regex("\\$".r) final case class `$`(text: String, span: Span.Range) extends Terminal
@regex("\"".r) final case class `"`(text: String, span: Span.Range) extends Terminal
@regex("'".r) final case class `'`(text: String, span: Span.Range) extends Terminal
@regex(">".r) final case class `>`(text: String, span: Span.Range) extends Terminal
@regex("<".r) final case class `<`(text: String, span: Span.Range) extends Terminal

// =====| Script |=====

/**
 * Minimal bash-like script subset:
 *   - variable assignment: NAME=WORD
 *   - pipelines: cmd | cmd | ...
 *   - statements separated by `;`
 *   - words: bare, double-quoted, single-quoted, $VAR
 *   - simple redirects on a command: > file / < file
 */
final case class Script(
    statements: ElementList[Statement],
) extends NonTerminal {
  override val span: Span.Range = statements match {
    case n: NonEmptyElementList[?] => n.span
    case n: ElementNil             => n.span
  }
}
object Script {

  val parser: Parser[Script] =
    new Parser[Script] {
      override def parse(source: Source): Either[ParseError, Script] = ???
    }

}

sealed trait Statement extends NonTerminal

final case class AssignStmt(
    name: Ident,
    eq: `=`,
    value: Word,
    semi: ElementOption[`;`],
) extends Statement {
  override val span: Span.Range =
    semi.toOption.map(s => name.span <> s.span).getOrElse(name.span <> value.span)
}

final case class PipelineStmt(
    pipeline: Pipeline,
    semi: ElementOption[`;`],
) extends Statement {
  override val span: Span.Range =
    semi.toOption.map(s => pipeline.span <> s.span).getOrElse(pipeline.span)
}

final case class Pipeline(
    head: Command,
    tail: ElementList[PipeCmd],
) extends NonTerminal {
  override val span: Span.Range =
    tail.headOption match {
      case Some(_) =>
        tail match {
          case n: NonEmptyElementList[?] => head.span <> n.span
          case n: ElementNil             => head.span <> n.span
        }
      case None => head.span
    }
}

final case class PipeCmd(
    pipe: Pipe,
    cmd: Command,
) extends NonTerminal {
  override val span: Span.Range = pipe.span <> cmd.span
}

final case class Command(
    name: Word,
    args: ElementList[Word],
    redirect: ElementOption[Redirect],
) extends NonTerminal {
  override val span: Span.Range = {
    val withArgs = args.headOption match {
      case Some(_) =>
        args match {
          case n: NonEmptyElementList[?] => name.span <> n.span
          case n: ElementNil             => name.span
        }
      case None => name.span
    }
    redirect.toOption.map(r => withArgs <> r.span).getOrElse(withArgs)
  }
}

sealed trait Redirect extends NonTerminal
final case class RedirOut(op: `>`, target: Word) extends Redirect {
  override val span: Span.Range = op.span <> target.span
}
final case class RedirIn(op: `<`, target: Word) extends Redirect {
  override val span: Span.Range = op.span <> target.span
}

// =====| Words |=====

sealed trait Word extends Element { self: Terminal | NonTerminal => }

@regex("[A-Za-z_][A-Za-z_0-9]*".r)
final case class Ident(text: String, span: Span.Range) extends Terminal

@regex("""[^\s|;$"'<>]+""".r)
final case class BareWord(text: String, span: Span.Range) extends Word, Terminal

final case class DoubleQuoted(
    open: `"`,
    parts: ElementList[DQuotePart],
    close: `"`,
) extends Word,
      NonTerminal {
  override val span: Span.Range = open.span <> close.span
}

sealed trait DQuotePart extends Element { self: Terminal | NonTerminal => }

@regex("""[^"$\\]+""".r)
final case class DQuoteChars(text: String, span: Span.Range) extends DQuotePart, Terminal

@regex("\\\\.".r)
final case class DQuoteEsc(text: String, span: Span.Range) extends DQuotePart, Terminal

final case class DQuoteVar(
    dollar: `$`,
    name: Ident,
) extends DQuotePart,
      NonTerminal {
  override val span: Span.Range = dollar.span <> name.span
}

final case class SingleQuoted(
    open: `'`,
    chars: SQuoteChars,
    close: `'`,
) extends Word,
      NonTerminal {
  override val span: Span.Range = open.span <> close.span
}

@regex("[^']*".r)
final case class SQuoteChars(text: String, span: Span.Range) extends Terminal

final case class VarRef(
    dollar: `$`,
    name: Ident,
) extends Word,
      NonTerminal {
  override val span: Span.Range = dollar.span <> name.span
}
