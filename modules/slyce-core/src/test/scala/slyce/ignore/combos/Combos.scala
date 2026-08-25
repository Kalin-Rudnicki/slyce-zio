package slyce.ignore.combos

import oxygen.predef.core.*

import slyce.core.*
import slyce.parse.*

/** =====| Annotation-combination grammars (self-contained package) |=====
  *
  * NOTE: terminals are declared in THIS file. A grammar's `@regex` terminals MUST live in the same
  * source file as its `Parser.derived` call — referencing a terminal from another file crashes the
  * derivation macro (`AssertionError: start of NoSpan`, see suite report). Hence each model package
  * re-declares the terminals it needs.
  *
  * Each grammar is the SAME 3-field product `x , y` (fields: `x`, `comma`, `y`) so the only variable
  * is WHICH ignore annotations are present. Gaps, for fields [x, comma, y]:
  *   - `before`  → leading (before `x`)
  *   - `between` → trailing on each NON-last field → after `x`, after `comma`
  *   - `after`   → trailing on the last field → after `y`
  */

// =====| Terminals |=====

@regex("[ \\t\\n\\r]+".r) final case class Ws(text: String, span: Span.Range) extends Terminal
@regex("[ \\t]+".r) final case class WsNoNl(text: String, span: Span.Range) extends Terminal
@regex(",".r) final case class `,`(text: String, span: Span.Range) extends Terminal
@regex("/\\*([^*]|\\*[^/])*\\*/".r) final case class BlockComment(text: String, span: Span.Range) extends Terminal

@regex("[0-9]+".r) final case class Num(text: String, span: Span.Range, value: BigInt) extends Terminal
object Num {
  given BuildTerminal[Num] = BuildTerminal.attemptDecode1(BigInt(_))(Num.apply)
}

private def pairSpan(x: Num, y: Num): Span.Range = x.span <> y.span

// =====| each annotation ALONE |=====

@ignoreBefore[Ws]
final case class BeforePair(x: Num, comma: `,`, y: Num) extends NonTerminal {
  override val span: Span.Range = pairSpan(x, y)
}
object BeforePair { val parser: Parser[BeforePair] = Parser.derived[BeforePair](2) }

@ignoreBetween[Ws]
final case class BetweenPair(x: Num, comma: `,`, y: Num) extends NonTerminal {
  override val span: Span.Range = pairSpan(x, y)
}
object BetweenPair { val parser: Parser[BetweenPair] = Parser.derived[BetweenPair](2) }

@ignoreAfter[Ws]
final case class AfterPair(x: Num, comma: `,`, y: Num) extends NonTerminal {
  override val span: Span.Range = pairSpan(x, y)
}
object AfterPair { val parser: Parser[AfterPair] = Parser.derived[AfterPair](2) }

// =====| pairwise combos |=====

@ignoreBefore[Ws] @ignoreAfter[Ws]
final case class BeforeAfterPair(x: Num, comma: `,`, y: Num) extends NonTerminal {
  override val span: Span.Range = pairSpan(x, y)
}
object BeforeAfterPair { val parser: Parser[BeforeAfterPair] = Parser.derived[BeforeAfterPair](2) }

@ignoreBetween[Ws] @ignoreAfter[Ws]
final case class BetweenAfterPair(x: Num, comma: `,`, y: Num) extends NonTerminal {
  override val span: Span.Range = pairSpan(x, y)
}
object BetweenAfterPair { val parser: Parser[BetweenAfterPair] = Parser.derived[BetweenAfterPair](2) }

@ignoreBefore[Ws] @ignoreBetween[Ws]
final case class BeforeBetweenPair(x: Num, comma: `,`, y: Num) extends NonTerminal {
  override val span: Span.Range = pairSpan(x, y)
}
object BeforeBetweenPair { val parser: Parser[BeforeBetweenPair] = Parser.derived[BeforeBetweenPair](2) }

// =====| single-field product: @ignoreBetween is a NO-OP |=====

@ignoreBetween[Ws]
final case class SoloBetween(n: Num) extends NonTerminal {
  override val span: Span.Range = n.span
}
object SoloBetween { val parser: Parser[SoloBetween] = Parser.derived[SoloBetween](2) }

@ignoreBefore[Ws]
final case class SoloBefore(n: Num) extends NonTerminal {
  override val span: Span.Range = n.span
}
object SoloBefore { val parser: Parser[SoloBefore] = Parser.derived[SoloBefore](2) }

@ignoreAfter[Ws]
final case class SoloAfter(n: Num) extends NonTerminal {
  override val span: Span.Range = n.span
}
object SoloAfter { val parser: Parser[SoloAfter] = Parser.derived[SoloAfter](2) }

// =====| whitespace that EXCLUDES newlines |=====

@ignoreBefore[WsNoNl] @ignoreBetween[WsNoNl] @ignoreAfter[WsNoNl]
final case class NoNlPair(x: Num, comma: `,`, y: Num) extends NonTerminal {
  override val span: Span.Range = pairSpan(x, y)
}
object NoNlPair { val parser: Parser[NoNlPair] = Parser.derived[NoNlPair](2) }

// =====| single-terminal comment-only ignore |=====

@ignoreBefore[BlockComment] @ignoreBetween[BlockComment] @ignoreAfter[BlockComment]
final case class CommentPair(x: Num, comma: `,`, y: Num) extends NonTerminal {
  override val span: Span.Range = pairSpan(x, y)
}
object CommentPair { val parser: Parser[CommentPair] = Parser.derived[CommentPair](2) }
