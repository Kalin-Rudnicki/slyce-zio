package slyce.ignore.nullables

import oxygen.predef.core.*

import slyce.core.*
import slyce.core.builtIn.*
import slyce.parse.*

/** =====| Nullable fields interacting with ignore (self-contained package) |=====
  *
  * Trailing-fold: `ElementOption[B]` → `(B T*)?`, `ElementList[B]` → `(B T*)*`,
  * `NonEmptyElementList[B]` → `(B T*)+`. An ABSENT nullable contributes zero ignore runs (no two
  * adjacent runs); a present list gets list-INTERNAL spacing for free.
  */

// =====| Terminals |=====

@regex("[ \\t\\n\\r]+".r) final case class Ws(text: String, span: Span.Range) extends Terminal
@regex("\\(".r) final case class `(`(text: String, span: Span.Range) extends Terminal
@regex("\\)".r) final case class `)`(text: String, span: Span.Range) extends Terminal
@regex(",".r) final case class `,`(text: String, span: Span.Range) extends Terminal
@regex(";".r) final case class `;`(text: String, span: Span.Range) extends Terminal
@regex("-".r) final case class `-`(text: String, span: Span.Range) extends Terminal

@regex("[0-9]+".r) final case class Num(text: String, span: Span.Range, value: BigInt) extends Terminal
object Num {
  given BuildTerminal[Num] = BuildTerminal.attemptDecode1(BigInt(_))(Num.apply)
}

// =====| nullable OPTION in FIRST position: `a? , b` |=====

@ignoreBefore[Ws] @ignoreBetween[Ws] @ignoreAfter[Ws]
final case class OptFirst(a: ElementOption[Num], comma: `,`, b: Num) extends NonTerminal {
  override val span: Span.Range = a.toOption.map(_.span).getOrElse(comma.span) <> b.span
}
object OptFirst { val parser: Parser[OptFirst] = Parser.derived[OptFirst](2) }

// =====| nullable OPTION in LAST position: `a , b?` |=====

@ignoreBefore[Ws] @ignoreBetween[Ws] @ignoreAfter[Ws]
final case class OptLast(a: Num, comma: `,`, b: ElementOption[Num]) extends NonTerminal {
  override val span: Span.Range = a.span <> b.toOption.map(_.span).getOrElse(comma.span)
}
object OptLast { val parser: Parser[OptLast] = Parser.derived[OptLast](2) }

// =====| nullable LIST in FIRST position: `items* ;` |=====

@ignoreBefore[Ws] @ignoreBetween[Ws] @ignoreAfter[Ws]
final case class ListFirst(items: ElementList[Num], end: `;`) extends NonTerminal {
  override val span: Span.Range = items.headOption.map(_.span).getOrElse(end.span) <> end.span
}
object ListFirst { val parser: Parser[ListFirst] = Parser.derived[ListFirst](2) }

// =====| NON-EMPTY list under ignore: `( items+ )` — empty must FAIL |=====

@ignoreBefore[Ws] @ignoreBetween[Ws] @ignoreAfter[Ws]
final case class NelCell(open: `(`, items: NonEmptyElementList[Num], close: `)`) extends NonTerminal {
  override val span: Span.Range = open.span <> close.span
}
object NelCell { val parser: Parser[NelCell] = Parser.derived[NelCell](2) }

// =====| ADJACENT nullable fields, DISTINCT terminal types: `( a? b? )` |=====

@ignoreBefore[Ws] @ignoreBetween[Ws] @ignoreAfter[Ws]
final case class AdjOpt(open: `(`, a: ElementOption[`-`], b: ElementOption[Num], close: `)`) extends NonTerminal {
  override val span: Span.Range = open.span <> close.span
}
object AdjOpt { val parser: Parser[AdjOpt] = Parser.derived[AdjOpt](2) }

// =====| PROBE: NonEmptyElementList WITHOUT ignore (to scope the empty-accept defect) |=====

final case class PlainNel(open: `(`, items: NonEmptyElementList[Num], close: `)`) extends NonTerminal {
  override val span: Span.Range = open.span <> close.span
}
object PlainNel { val parser: Parser[PlainNel] = Parser.derived[PlainNel](2) }

// =====| OPTION followed by a required field, distinct types: `( a? n )` |=====

@ignoreBefore[Ws] @ignoreBetween[Ws] @ignoreAfter[Ws]
final case class OptThenReq(open: `(`, a: ElementOption[`-`], n: Num, close: `)`) extends NonTerminal {
  override val span: Span.Range = open.span <> close.span
}
object OptThenReq { val parser: Parser[OptThenReq] = Parser.derived[OptThenReq](2) }
