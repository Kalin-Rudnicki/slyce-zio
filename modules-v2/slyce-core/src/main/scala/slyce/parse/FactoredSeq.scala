package slyce.parse

import slyce.core.*
import slyce.core.builtIn.*

/**
 * Runtime value for a left-factored sequence that replaces two adjacent grammar
 * symbols (typically a list and a following nullable) that shared a FIRST set.
 *
 * Produced by grammar rewrite; consumed when building the original product ADT
 * (list field + trail field, possibly through single-field product wrappers).
 */
final case class FactoredSeq(
    list: ElementList[Element],
    trail: ElementOption[Element],
)

/** Intermediate reduce value for the tail phase of a left-factored sequence. */
enum FactoredTail {
  /** Shared terminal was only the trailing optional (no list element). */
  case TrailOnly
  /** Shared terminal began a list element; `rest` is the element's remaining fields. */
  case Continues(rest: IArray[Any], cont: FactoredSeq)
}
