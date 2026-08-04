package slyce.generate.grammar

import oxygen.predef.core.*

/** High-level NT before expansion to flat RawNT productions. */
enum NTGroup {
  case BasicNT(name: GSym.Nt, prods: NonEmptyList[List[GSym]])
  case ListNT(id: String, elem: GSym, nonempty: Boolean)
  case Optional(child: GSym)

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
    }

}
