package slyce.ignore.model

import oxygen.predef.core.*

import slyce.core.*
import slyce.parse.*

// =====| Terminals |=====

@regex("[ \\t\\n\\r]+".r) final case class Ws(text: String, span: Span.Range) extends Terminal
@regex("\\(".r) final case class `(`(text: String, span: Span.Range) extends Terminal
@regex("\\)".r) final case class `)`(text: String, span: Span.Range) extends Terminal
@regex(",".r) final case class `,`(text: String, span: Span.Range) extends Terminal

@regex("[0-9]+".r) final case class Num(text: String, span: Span.Range, value: BigInt) extends Terminal
object Num {
  given BuildTerminal[Num] = BuildTerminal.attemptDecode1(BigInt(_))(Num.apply)
}

// =====| Point — `( x , y )` with whitespace ignorable anywhere |=====

/** `@ignoreBefore/Between/After[Ws]` inject discardable `Ws*` slots at every gap, so any amount of
  * whitespace may surround the structure and sit between consecutive fields — none of it becomes a field.
  */
@ignoreBefore[Ws] @ignoreBetween[Ws] @ignoreAfter[Ws]
final case class Point(
    open: `(`,
    x: Num,
    comma: `,`,
    y: Num,
    close: `)`,
) extends NonTerminal {
  override val span: Span.Range = open.span <> close.span
}
object Point {
  val parser: Parser[Point] = Parser.derived[Point](2)
}

// =====| Comments — a sum-terminal ignore (whitespace + line + block comments) |=====

/** The ignore terminal can be a sealed sum of terminals — so runs of interleaved whitespace and
  * comments are all discarded. This is the shape a real language (e.g. ambrosia) needs.
  */
sealed trait Noise extends Terminal
@regex("[ \\t\\n\\r]+".r) final case class NWs(text: String, span: Span.Range) extends Noise
@regex("//[^\\n]*".r) final case class LineComment(text: String, span: Span.Range) extends Noise
@regex("/\\*([^*]|\\*[^/])*\\*/".r) final case class BlockComment(text: String, span: Span.Range) extends Noise

@ignoreBefore[Noise] @ignoreBetween[Noise] @ignoreAfter[Noise]
final case class CPoint(
    open: `(`,
    x: Num,
    comma: `,`,
    y: Num,
    close: `)`,
) extends NonTerminal {
  override val span: Span.Range = open.span <> close.span
}
object CPoint {
  val parser: Parser[CPoint] = Parser.derived[CPoint](2)
}

// =====| Nullable fields under ignore (the review's #1 hazard) |=====

/** Optional trailing field. `b` absent must NOT create two adjacent ignore runs. */
@ignoreBefore[Ws] @ignoreBetween[Ws] @ignoreAfter[Ws]
final case class OptCell(
    open: `(`,
    a: Num,
    b: slyce.core.builtIn.ElementOption[Num],
    close: `)`,
) extends NonTerminal {
  override val span: Span.Range = open.span <> close.span
}
object OptCell {
  val parser: Parser[OptCell] = Parser.derived[OptCell](2)
}

/** Nullable list field. Empty list must be clean; non-empty exercises list-INTERNAL spacing. */
@ignoreBefore[Ws] @ignoreBetween[Ws] @ignoreAfter[Ws]
final case class ListCell(
    open: `(`,
    items: slyce.core.builtIn.ElementList[Num],
    close: `)`,
) extends NonTerminal {
  override val span: Span.Range = open.span <> close.span
}
object ListCell {
  val parser: Parser[ListCell] = Parser.derived[ListCell](2)
}

// =====| Sum-child product with ignore (the review's #2 untested path) |=====

sealed trait Wrapped extends NonTerminal
object Wrapped {
  val parser: Parser[Wrapped] = Parser.derived[Wrapped](2)
}

@ignoreBefore[Ws] @ignoreBetween[Ws] @ignoreAfter[Ws]
final case class Boxed(open: `(`, n: Num, close: `)`) extends Wrapped {
  override val span: Span.Range = open.span <> close.span
}
