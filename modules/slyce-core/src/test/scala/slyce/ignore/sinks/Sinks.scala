package slyce.ignore.sinks

import oxygen.predef.core.*

import slyce.core.*
import slyce.core.builtIn.*
import slyce.parse.*

/** =====| Kitchen-sink grammars (self-contained package) |=====
  *
  * Realistic mini-languages that ignore whitespace + comments (`Noise`) EVERYWHERE while nesting
  * options, lists, and sums.
  *
  * Placement discipline (a finding in its own right): per-product (Option-A) ignore has NO cascade,
  * and two adjacent ignore runs (`Noise* Noise*`) at a boundary is an LR conflict that fails the table
  * build. Two rules keep these grammars conflict-free:
  *   1. the OUTERMOST product carries `before`/`after`; every NESTED product carries `@ignoreBetween`
  *      ONLY, so its entry/exit gaps are owned by the enclosing product.
  *   2. do NOT wrap, in an `ElementOption`, a nested product that itself ends in an ignore run — the
  *      parent's fold `(Child Noise*)?` then collides with the child's own trailing run. Instead INLINE
  *      the body (a leading `ElementOption[value]` + trailing `ElementList[comma-value]`) directly into
  *      the container, so no nested product ends in ignore.
  */

// =====| Shared terminals |=====

@regex("[ \\t\\n\\r]+".r) final case class NWs(text: String, span: Span.Range) extends Noise
@regex("//[^\\n]*".r) final case class LineComment(text: String, span: Span.Range) extends Noise
@regex("/\\*([^*]|\\*[^/])*\\*/".r) final case class BlockComment(text: String, span: Span.Range) extends Noise
sealed trait Noise extends Terminal

@regex("\\[".r) final case class `[`(text: String, span: Span.Range) extends Terminal
@regex("\\]".r) final case class `]`(text: String, span: Span.Range) extends Terminal
@regex("\\{".r) final case class `{`(text: String, span: Span.Range) extends Terminal
@regex("\\}".r) final case class `}`(text: String, span: Span.Range) extends Terminal
@regex("\\(".r) final case class `(`(text: String, span: Span.Range) extends Terminal
@regex("\\)".r) final case class `)`(text: String, span: Span.Range) extends Terminal
@regex(",".r) final case class `,`(text: String, span: Span.Range) extends Terminal
@regex(":".r) final case class `:`(text: String, span: Span.Range) extends Terminal
@regex(";".r) final case class `;`(text: String, span: Span.Range) extends Terminal
@regex("[a-zA-Z_][a-zA-Z0-9_]*".r) final case class Id(text: String, span: Span.Range) extends Terminal

@regex("-?(?:0|[1-9][0-9]*)".r)
final case class JNum(text: String, span: Span.Range, value: BigInt) extends JVal, Terminal
object JNum {
  given BuildTerminal[JNum] = BuildTerminal.attemptDecode1(BigInt(_))(JNum.apply)
}

// =====| Sink 1 — JSON-ish value language, whitespace + comments ignored everywhere |=====

sealed trait JVal extends Element { self: Terminal | NonTerminal => }

/** Root wrapper: owns the OUTER noise (before + after). */
@ignoreBefore[Noise] @ignoreAfter[Noise]
final case class JDoc(value: JVal) extends NonTerminal {
  override val span: Span.Range = value.span
}
object JDoc { val parser: Parser[JDoc] = Parser.derived[JDoc](2) }

/** `[ v , v , v ]` — body inlined (head option + tail list) to avoid adjacent-noise conflicts. */
@ignoreBetween[Noise]
final case class JArr(
    open: `[`,
    head: ElementOption[JVal],
    tail: ElementList[JComma],
    close: `]`,
) extends JVal,
      NonTerminal {
  override val span: Span.Range = open.span <> close.span
  def values: List[JVal] = head.toOption.toList ::: tail.toList.map(_.value)
}

@ignoreBetween[Noise]
final case class JComma(comma: `,`, value: JVal) extends NonTerminal {
  override val span: Span.Range = comma.span <> value.span
}

/** `{ k : v , k : v }` — body inlined the same way. */
@ignoreBetween[Noise]
final case class JObj(
    open: `{`,
    head: ElementOption[JPair],
    tail: ElementList[JCommaPair],
    close: `}`,
) extends JVal,
      NonTerminal {
  override val span: Span.Range = open.span <> close.span
  def pairs: List[JPair] = head.toOption.toList ::: tail.toList.map(_.pair)
}

@ignoreBetween[Noise]
final case class JPair(key: Id, colon: `:`, value: JVal) extends NonTerminal {
  override val span: Span.Range = key.span <> value.span
}

@ignoreBetween[Noise]
final case class JCommaPair(comma: `,`, pair: JPair) extends NonTerminal {
  override val span: Span.Range = comma.span <> pair.span
}

// =====| Sink 2 — tiny function language: `name(a, b, c) { s; s; }` with comments |=====

@ignoreBefore[Noise] @ignoreBetween[Noise] @ignoreAfter[Noise]
final case class Func(
    name: Id,
    open: `(`,
    firstParam: ElementOption[Id],
    restParams: ElementList[CommaId],
    close: `)`,
    body: Block,
) extends NonTerminal {
  override val span: Span.Range = name.span <> body.span
  def params: List[Id] = firstParam.toOption.toList ::: restParams.toList.map(_.id)
}
object Func { val parser: Parser[Func] = Parser.derived[Func](2) }

@ignoreBetween[Noise]
final case class CommaId(comma: `,`, id: Id) extends NonTerminal {
  override val span: Span.Range = comma.span <> id.span
}

@ignoreBetween[Noise]
final case class Block(open: `{`, stmts: ElementList[Stmt], close: `}`) extends NonTerminal {
  override val span: Span.Range = open.span <> close.span
}

@ignoreBetween[Noise]
final case class Stmt(name: Id, semi: `;`) extends NonTerminal {
  override val span: Span.Range = name.span <> semi.span
}

// =====| Sink 3 — comma-separated list of records with an optional field |=====

@ignoreBefore[Noise] @ignoreBetween[Noise] @ignoreAfter[Noise]
final case class RecordList(head: Record, tail: ElementList[CommaRecord]) extends NonTerminal {
  override val span: Span.Range = tail.toList.lastOption.fold(head.span)(head.span <> _.span)
  def records: List[Record] = head :: tail.toList.map(_.record)
}
object RecordList { val parser: Parser[RecordList] = Parser.derived[RecordList](2) }

@ignoreBetween[Noise]
final case class CommaRecord(comma: `,`, record: Record) extends NonTerminal {
  override val span: Span.Range = comma.span <> record.span
}

@ignoreBetween[Noise]
final case class Record(open: `{`, key: Id, colon: `:`, value: ElementOption[JNum], close: `}`) extends NonTerminal {
  override val span: Span.Range = open.span <> close.span
}
