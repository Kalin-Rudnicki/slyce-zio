package slyce.generate.grammar

import slyce.core.*

final case class NamedNonTerminal(
    name: Marked[Identifier.NonTerminal],
    nt: NonTerminal,
)
