package slyce.generate.grammar

import slyce.core.*

final case class GrammarInput(
    startNT: Marked[String],
    nonTerminals: List[NamedNonTerminal],
)
