package slyce.json.model

import oxygen.predef.core.*

import slyce.core.*
import slyce.core.builtIn.*
import slyce.parse.*

// =====| Punctuation |=====

@regex("\\{".r) final case class `{`(text: String, span: Span.Range) extends Terminal
@regex("\\}".r) final case class `}`(text: String, span: Span.Range) extends Terminal
@regex("\\[".r) final case class `[`(text: String, span: Span.Range) extends Terminal
@regex("\\]".r) final case class `]`(text: String, span: Span.Range) extends Terminal
@regex(",".r) final case class `,`(text: String, span: Span.Range) extends Terminal
@regex(":".r) final case class `:`(text: String, span: Span.Range) extends Terminal
@regex("\"".r) final case class `"`(text: String, span: Span.Range) extends Terminal

// =====| Json |=====

/** JSON value (terminals + composite nonterminals). */
sealed trait Json extends Element { self: Terminal | NonTerminal => }
object Json {

  val parser: Parser[Json] =
    new Parser[Json] {
      override def parse(source: Source): Either[ParseError, Json] = ???
    }

}

@regex("null".r)
final case class NullLit(text: String, span: Span.Range) extends Json, Terminal

@regex("true".r)
final case class TrueLit(text: String, span: Span.Range) extends Json, Terminal

@regex("false".r)
final case class FalseLit(text: String, span: Span.Range) extends Json, Terminal

@regex("-?(?:0|[1-9]\\d*)".r)
final case class IntLit(text: String, span: Span.Range, value: BigInt) extends Json, Terminal
object IntLit {
  given BuildTerminal[IntLit] = BuildTerminal.attemptDecode1(BigInt(_))(IntLit.apply)
}

@regex("-?(?:0|[1-9]\\d*)\\.\\d+".r)
final case class FloatLit(text: String, span: Span.Range, value: BigDecimal) extends Json, Terminal
object FloatLit {
  given BuildTerminal[FloatLit] = BuildTerminal.attemptDecode1(BigDecimal(_))(FloatLit.apply)
}

// =====| String |=====

final case class Str(
    open: `"`,
    parts: ElementList[StrPart],
    close: `"`,
) extends Json,
      NonTerminal {
  override val span: Span.Range = open.span <> close.span
}

sealed trait StrPart extends Terminal

@regex("[^\\n\"\\\\]+".r)
final case class Chars(text: String, span: Span.Range) extends StrPart

@regex("\\\\.".r)
final case class EscChar(text: String, span: Span.Range, char: Char) extends StrPart
object EscChar {

  private def convert(c: Char): Either[String, Char] = c match
    case '\\' => '\\'.asRight
    case '"'  => '"'.asRight
    case 'n'  => '\n'.asRight
    case 't'  => '\t'.asRight
    case 'r'  => '\r'.asRight
    case '/'  => '/'.asRight
    case _    => s"Invalid escape: \\$c".asLeft

  given BuildTerminal[EscChar] =
    (text, span) =>
      if text.length == 2 && text(0) == '\\' then convert(text(1)).map(EscChar(text, span, _))
      else "Malformed escape".asLeft

}

// =====| Array |=====

final case class Arr(
    open: `[`,
    body: ElementOption[ArrBody],
    close: `]`,
) extends Json,
      NonTerminal {
  override val span: Span.Range = open.span <> close.span
}

final case class ArrBody(
    head: Json,
    tail: ElementList[CommaJson],
) extends NonTerminal {
  override val span: Span.Range =
    tail.headOption match {
      case Some(last) => head.span <> last.span
      case None       => head.span
    }
}

final case class CommaJson(
    comma: `,`,
    value: Json,
) extends NonTerminal {
  override val span: Span.Range = comma.span <> value.span
}

// =====| Object |=====

final case class Obj(
    open: `{`,
    body: ElementOption[ObjBody],
    close: `}`,
) extends Json,
      NonTerminal {
  override val span: Span.Range = open.span <> close.span
}

final case class ObjBody(
    head: KeyPair,
    tail: ElementList[CommaKeyPair],
) extends NonTerminal {
  override val span: Span.Range =
    tail.headOption match {
      case Some(last) => head.span <> last.span
      case None       => head.span
    }
}

final case class KeyPair(
    key: Str,
    colon: `:`,
    value: Json,
) extends NonTerminal {
  override val span: Span.Range = key.span <> value.span
}

final case class CommaKeyPair(
    comma: `,`,
    pair: KeyPair,
) extends NonTerminal {
  override val span: Span.Range = comma.span <> pair.span
}
