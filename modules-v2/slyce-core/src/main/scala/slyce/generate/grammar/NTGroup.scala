package slyce.generate.grammar

import oxygen.predef.core.*

/** High-level NT before expansion to flat RawNT productions. */
enum NTGroup {
  case BasicNT(name: GSym.Nt, prods: NonEmptyList[List[GSym]])
  case ListNT(id: String, elem: GSym, nonempty: Boolean)
  case Optional(child: GSym)
  /**
   * Left-factored replacement for `List[E] Opt[T]` (and unit-NT chains to that shape)
   * when E's unique production is `T · rest…` with non-empty `rest`.
   */
  case LeftFactoredSeq(
      id: String,
      sharedTerm: GSym.Term,
      elemRest: List[GSym],
  )

  final lazy val rawNTs: NonEmptyList[RawNT] = NTGroup.toRaw(this)
}
object NTGroup {

  def toRaw(group: NTGroup): NonEmptyList[RawNT] =
    group match {
      case NTGroup.BasicNT(name, prods) =>
        NonEmptyList.one(RawNT(name, prods.map(Production(_))))
      case NTGroup.ListNT(id, elem, nonempty) =>
        if nonempty then {
          val head = GSym.ListNt(id, GSym.ListPhase.Head)
          val tail = GSym.ListNt(id, GSym.ListPhase.Tail)
          val cons = Production(elem, tail)
          NonEmptyList.of(
            RawNT(head, NonEmptyList.one(cons)),
            RawNT(tail, NonEmptyList.of(cons, Production())),
          )
        } else {
          val simple = GSym.ListNt(id, GSym.ListPhase.Simple)
          NonEmptyList.one(
            RawNT(simple, NonEmptyList.of(Production(elem, simple), Production())),
          )
        }
      case NTGroup.Optional(child) =>
        val name = GSym.OptNt(child.label)
        NonEmptyList.one(
          RawNT(name, NonEmptyList.of(Production(child), Production())),
        )
      case NTGroup.LeftFactoredSeq(id, sharedTerm, elemRest) =>
        val head = GSym.SeqNt(id, GSym.SeqPhase.Head)
        val tail = GSym.SeqNt(id, GSym.SeqPhase.Tail)
        NonEmptyList.of(
          RawNT(head, NonEmptyList.of(Production(sharedTerm, tail), Production())),
          RawNT(tail, NonEmptyList.of(Production((elemRest :+ head)*), Production())),
        )
    }

}
