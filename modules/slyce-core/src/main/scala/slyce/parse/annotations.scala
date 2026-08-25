package slyce.parse

import scala.annotation.Annotation
import scala.util.matching.Regex

import slyce.core.*

final case class regex(r: Regex) extends Annotation

/** OPT-IN lexer tie-break priority for a terminal. Higher wins.
  *
  * Priority ONLY breaks **equal-length** lexer ties; longest-match (maximal munch) is always the primary rule and is never overridden by priority. Default (no `@priority`) is `0`, which reproduces
  * the historical behavior exactly: equal-length ties among equal-priority terminals fall back to first-match-wins.
  *
  * The motivating use is a reserved keyword that overlaps an identifier regex: give the keyword terminal a higher priority than `Identifier` so the exact keyword string lexes as the keyword, while an
  * identifier that merely starts with the keyword (e.g. `structure` vs `struct`) still lexes as one identifier because the longer match wins first.
  *
  * {{{
  * @priority(1) @regex("struct".r) final case class KwStruct(text: String, span: Span.Range) extends Terminal
  * @regex("[A-Za-z][A-Za-z0-9_]*".r) final case class Identifier(text: String, span: Span.Range) extends Terminal
  * }}}
  *
  * Notes:
  *   - SHADOWING: on an equal-length overlap, the higher-priority terminal simply wins; the lower-priority one is silently shadowed on that exact input. NO author-facing diagnostic is emitted for the
  *     shadowed terminal — it is up to the author to intend the ranking.
  *   - LITERAL Int only: `@priority(n)` takes a compile-time-constant `Int`. NEGATIVE values are permitted and rank BELOW the default `0`, so a terminal can be pushed under un-annotated peers.
  *   - DETERMINISM SCOPE: priority makes a tie deterministic only for an overlap the grammar validator actually detects (a finite-sample heuristic, see `GrammarValidity.regexesCanBothMatch`). A
  *     genuine equal-length, equal-priority collision the sample misses is not flagged and falls back at runtime to first-match-wins (dependent on lexer `allowed`-set / Set iteration order —
  *     pre-existing, not introduced by priority). Priority does not claim to make ALL ties order-independent, only the specific one it is given a strict ranking on.
  */
final case class priority(n: Int) extends Annotation

// Ignore annotations inject discardable ignore slots into a product's RHS at the file edges (`Before`,
// `After`) and between consecutive fields (`Between`). Two families:
//
//  - `ignoreBefore/After/Between[A]` emit a `A*` (Kleene STAR) slot — use when `A` is a SUM of ignore
//    terminals (or otherwise can appear as several consecutive tokens), so a run may be many tokens.
//
//  - `ignoreBefore/After/BetweenOne[A]` emit a `A?` (OPTIONAL, 0-or-1) slot — use when `A` is a SINGLE
//    maximal-munch terminal (its regex greedily consumes a whole whitespace/comment run, so at most ONE
//    `A` ever sits in a gap). The bounded slot keeps look-ahead finite, which the star form cannot: the
//    generator can't see that a lexer is maximal-munch, so a `A*` slot forces it to look past an
//    unbounded run (LR(k) never converges for grammars that must peek one token past a gap, e.g. infix
//    expressions). Prefer the `One` family whenever the ignore is a single maximal-munch terminal.
final case class ignoreBefore[A <: Terminal]() extends Annotation
final case class ignoreAfter[A <: Terminal]() extends Annotation
final case class ignoreBetween[A <: Terminal]() extends Annotation

final case class ignoreBeforeOne[A <: Terminal]() extends Annotation
final case class ignoreAfterOne[A <: Terminal]() extends Annotation
final case class ignoreBetweenOne[A <: Terminal]() extends Annotation
