package slyce.generate.grammar

import oxygen.predef.core.*

final case class RawNT(
    name: GSym.NonTerm,
    productions: NonEmptyList[Production],
)
