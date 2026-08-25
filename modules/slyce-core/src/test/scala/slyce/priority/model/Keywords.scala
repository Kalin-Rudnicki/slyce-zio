package slyce.priority.model

import slyce.core.*
import slyce.parse.*

// =====| Terminals |=====

@regex("[ \\t\\n\\r]+".r) final case class Ws(text: String, span: Span.Range) extends Terminal

/** `Tok` is a SUM of a reserved keyword and a general identifier whose regexes OVERLAP on the exact string `"struct"`. Without `@priority` this sum is a hard grammar error (ambiguous leading
  * terminals); the explicit priority difference is what lets it compile AND makes the equal-length lexer tie deterministic.
  */
sealed trait Tok extends Terminal

/** Keyword terminal that literally spells `struct` — higher priority than `Ident`. */
@priority(1) @regex("struct".r) final case class KwStruct(text: String, span: Span.Range) extends Tok

/** Identifier regex — DEFAULT priority (0). Overlaps `KwStruct` on `"struct"`, but is longer on `structure`. */
@regex("[A-Za-z][A-Za-z0-9_]*".r) final case class Ident(text: String, span: Span.Range) extends Tok

// =====| Grammars |=====

/** Single token. Proves, in isolation:
  *   - `"struct"` (equal-length tie) lexes as `KwStruct` — priority wins;
  *   - `"structure"` lexes as one `Ident` — longest-match beats priority (maximal munch preserved).
  */
final case class One(tok: Tok) extends NonTerminal {
  override val span: Span.Range = tok.span
}
object One {
  val parser: Parser[One] = Parser.derived[One](1)
}

/** Two whitespace-separated tokens. Proves the tie-break holds mid-stream and does not depend on which token came first (i.e. is independent of lexer `allowed`-set iteration order).
  */
@ignoreBefore[Ws] @ignoreBetween[Ws] @ignoreAfter[Ws]
final case class Pair(a: Tok, b: Tok) extends NonTerminal {
  override val span: Span.Range = a.span <> b.span
}
object Pair {
  val parser: Parser[Pair] = Parser.derived[Pair](2)
}

// =====| Prefix-literal siblings (back-compat, no priority) |=====

/** A SUM of two sibling literal keyword terminals whose regexes PREFIX-overlap (`in` is a strict prefix of `insert`) but never match the SAME input at the SAME length. This is NOT a lexer ambiguity —
  * maximal munch resolves it — so it must derive WITHOUT any `@priority` and WITHOUT a grammar error. (Before the equal-length fix to `GrammarValidity.regexesCanBothMatch`, this over-rejected and
  * `errorAndAbort`ed — a back-compat regression this model guards against by simply compiling.)
  */
sealed trait Kw2 extends Terminal

@regex("in".r) final case class KwIn(text: String, span: Span.Range) extends Kw2
@regex("insert".r) final case class KwInsert(text: String, span: Span.Range) extends Kw2

final case class One2(tok: Kw2) extends NonTerminal {
  override val span: Span.Range = tok.span
}
object One2 {
  val parser: Parser[One2] = Parser.derived[One2](1)
}

// =====| Genuine equal-length, equal-priority overlap (MUST hard-fail derivation) |=====

/** A SUM whose two terminals genuinely overlap at EQUAL length with EQUAL (default) priority: `struct` and an identifier regex both match `"struct"` ending at the same position. Deriving a parser for
  * this MUST hard-fail (`GrammarValidity` `errorAndAbort`). No `.parser` is defined here so the module still compiles; the derivation is exercised via `typeCheck` in `PrioritySpec`.
  */
sealed trait BadTok extends Terminal

@regex("struct".r) final case class BadKw(text: String, span: Span.Range) extends BadTok
@regex("[A-Za-z][A-Za-z0-9_]*".r) final case class BadIdent(text: String, span: Span.Range) extends BadTok

final case class BadOne(tok: BadTok) extends NonTerminal {
  override val span: Span.Range = tok.span
}
