package slyce.generate.grammar

import oxygen.predef.core.*

final case class ExpandedGrammar(
    startNt: GSym.Nt,
    maxLookAhead: Int,
    ntGroups: List[NTGroup],
) {
  lazy val rawNTs: List[RawNT] = ntGroups.flatMap(_.rawNTs.toList)

  lazy val productionsForNT: Map[GSym, List[(Int, Production)]] =
    rawNTs.map { nt =>
      nt.name -> nt.productions.toList.zipWithIndex.map { case (p, i) => (i, p) }
    }.toMap
}
