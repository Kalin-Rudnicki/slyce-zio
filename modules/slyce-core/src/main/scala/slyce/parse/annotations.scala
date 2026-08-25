package slyce.parse

import scala.annotation.Annotation
import scala.util.matching.Regex

import slyce.core.*

final case class regex(r: Regex) extends Annotation

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
