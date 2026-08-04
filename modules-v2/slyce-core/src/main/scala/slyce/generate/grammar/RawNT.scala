package slyce.generate.grammar

import oxygen.predef.core.*

final case class RawNT(
    name: GSym.Nt | GSym.ListNt | GSym.OptNt,
    productions: NonEmptyList[Production],
)
