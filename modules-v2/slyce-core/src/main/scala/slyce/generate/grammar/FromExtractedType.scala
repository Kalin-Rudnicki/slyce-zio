package slyce.generate.grammar

import java.util.UUID
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.collection.mutable
import scala.quoted.*

import slyce.generate.*

/**
 * Bridge ExtractedType tree → ExpandedGrammar.
 * Also records reduce metadata for macro codegen (product/list/opt construction).
 */
private[slyce] object FromExtractedType {

  enum ReduceKind {
    /** Case-class product: pop `arity` values, instantiate via ProductGeneric at macro time. */
    case Product(typeLabel: String, arity: Int)
    /** Production is a single child already of the desired type (e.g. sum of terminals). */
    case Identity
    case ListNil
    case ListCons
    case OptSome
    case OptNone
  }

  final case class Result(
      grammar: ExpandedGrammar,
      /** (ntLabel, prodIdx) → how to reduce */
      reduces: Map[(String, Int), ReduceKind],
      /** terminal typeLabel → ExtractedType.ProductTerminal (macro only) */
      terminals: Map[String, ExtractedType.ProductTerminal],
      /** product typeLabel → ExtractedType.ProductNonTerminal for reduce codegen */
      products: Map[String, ExtractedType.ProductNonTerminal],
  )

  def apply(
      root: ExtractedType,
      cache: ExtractedTypeCache,
      maxLookAhead: Int,
  )(using Quotes): Result = {
    val ctx = new Ctx(cache)
    val startSym = ctx.ref(root)
    startSym match {
      case nt: GSym.Nt =>
        Result(
          grammar = ExpandedGrammar(nt, maxLookAhead, ctx.groups.toList),
          reduces = ctx.reduces.toMap,
          terminals = ctx.terminals.toMap,
          products = ctx.products.toMap,
        )
      case other =>
        report.errorAndAbort(s"Parser root must be a named non-terminal, got: $other")
    }
  }

  private final class Ctx(cache: ExtractedTypeCache)(using Quotes) {
    val groups: mutable.ArrayBuffer[NTGroup] = mutable.ArrayBuffer.empty
    val reduces: mutable.Map[(String, Int), ReduceKind] = mutable.Map.empty
    val terminals: mutable.Map[String, ExtractedType.ProductTerminal] = mutable.Map.empty
    val products: mutable.Map[String, ExtractedType.ProductNonTerminal] = mutable.Map.empty

    private val seenNt: mutable.Set[String] = mutable.Set.empty
    private val listIds: mutable.Map[String, String] = mutable.Map.empty

    def labelOf(et: ExtractedType): String =
      et.typeRepr.showCode

    def ref(et: ExtractedType): GSym =
      et match {
        case t: ExtractedType.ProductTerminal =>
          val lab = labelOf(t)
          terminals.update(lab, t)
          GSym.Term(lab)

        case t: ExtractedType.ProductNonTerminal =>
          val lab = labelOf(t)
          ensureProduct(t)
          GSym.Nt(lab)

        case t: ExtractedType.SumNonTerminal =>
          val lab = labelOf(t)
          ensureSumNonTerminal(t)
          GSym.Nt(lab)

        case t: ExtractedType.SumTerminal =>
          val lab = labelOf(t)
          ensureSumTerminal(t)
          GSym.Nt(lab)

        case t: ExtractedType.SumElement =>
          val lab = labelOf(t)
          ensureSumElement(t)
          GSym.Nt(lab)

        case t: ExtractedType.ElementListBuiltIn =>
          listSym(t.elem, nonempty = false)

        case t: ExtractedType.NonEmptyElementListBuiltIn =>
          listSym(t.elem, nonempty = true)

        case t: ExtractedType.ElementOptionBuiltIn =>
          optSym(t.elem)

        case t: ExtractedType.IgnoreBuiltIn =>
          report.errorAndAbort("IgnoreBuiltIn fields are not supported yet")

        case t: ExtractedType.UnionBuiltIn =>
          report.errorAndAbort(s"UnionBuiltIn not supported yet: ${t.typeRepr.showCode}")

        case t: ExtractedType.VElementListBuiltIn =>
          report.errorAndAbort("VElementList not supported yet")

        case t: ExtractedType.NonEmptyVElementListBuiltIn =>
          report.errorAndAbort("NonEmptyVElementList not supported yet")
      }

    private def listSym(elemEt: ExtractedType, nonempty: Boolean): GSym = {
      val elem = ref(elemEt)
      val key = s"${elem.label}|${if nonempty then "+" else "*"}"
      val id = listIds.getOrElseUpdate(key, UUID.randomUUID().toString)
      val phase = if nonempty then GSym.ListPhase.Head else GSym.ListPhase.Simple
      if !seenNt.contains(s"list:$id") then {
        seenNt += s"list:$id"
        groups += NTGroup.ListNT(id, elem, nonempty)
        if nonempty then {
          reduces += ((GSym.ListNt(id, GSym.ListPhase.Head).label, 0) -> ReduceKind.ListCons)
          reduces += ((GSym.ListNt(id, GSym.ListPhase.Tail).label, 0) -> ReduceKind.ListCons)
          reduces += ((GSym.ListNt(id, GSym.ListPhase.Tail).label, 1) -> ReduceKind.ListNil)
        } else {
          reduces += ((GSym.ListNt(id, GSym.ListPhase.Simple).label, 0) -> ReduceKind.ListCons)
          reduces += ((GSym.ListNt(id, GSym.ListPhase.Simple).label, 1) -> ReduceKind.ListNil)
        }
      }
      GSym.ListNt(id, phase)
    }

    private def optSym(elemEt: ExtractedType): GSym = {
      val elem = ref(elemEt)
      val name = GSym.OptNt(elem.label)
      if !seenNt.contains(name.label) then {
        seenNt += name.label
        groups += NTGroup.Optional(elem)
        reduces += ((name.label, 0) -> ReduceKind.OptSome)
        reduces += ((name.label, 1) -> ReduceKind.OptNone)
      }
      name
    }

    private def ensureProduct(t: ExtractedType.ProductNonTerminal): Unit = {
      val lab = labelOf(t)
      if seenNt.contains(lab) then return
      seenNt += lab
      products.update(lab, t)

      val fieldSyms: List[GSym] = t.fields.toList.map(f => ref(f.extracted))
      groups += NTGroup.BasicNT(GSym.Nt(lab), NonEmptyList.one(fieldSyms))
      reduces += ((lab, 0) -> ReduceKind.Product(lab, fieldSyms.size))
    }

    /** Sum of nonterminals only (e.g. Expr.Add). */
    private def ensureSumNonTerminal(t: ExtractedType.SumNonTerminal): Unit = {
      val lab = labelOf(t)
      if seenNt.contains(lab) then return
      seenNt += lab
      emitSumProds(lab, t.directChildren.toList)
    }

    /** Sum of terminals only (e.g. StrPart). */
    private def ensureSumTerminal(t: ExtractedType.SumTerminal): Unit = {
      val lab = labelOf(t)
      if seenNt.contains(lab) then return
      seenNt += lab
      emitSumProds(lab, t.directChildren.toList)
    }

    /** Mixed Element sum (e.g. Json = terminals + nonterminals). */
    private def ensureSumElement(t: ExtractedType.SumElement): Unit = {
      val lab = labelOf(t)
      if seenNt.contains(lab) then return
      seenNt += lab
      emitSumProds(lab, t.directChildren.toList)
    }

    /**
     * One production per direct child:
     * - ProductTerminal → [Term] + Identity
     * - ProductNonTerminal → inlined fields + Product reduce
     * - nested Sum* → [ChildNt] + Identity (value already has parent type if <: Element)
     */
    private def emitSumProds(lab: String, children: List[ExtractedType]): Unit = {
      val prods: List[List[GSym]] =
        children.zipWithIndex.map { case (child, idx) =>
          child match {
            case c: ExtractedType.ProductTerminal =>
              val tLab = labelOf(c)
              terminals.update(tLab, c)
              reduces += ((lab, idx) -> ReduceKind.Identity)
              List(GSym.Term(tLab))

            case c: ExtractedType.ProductNonTerminal =>
              products.update(labelOf(c), c)
              // Prefer separate NT when product may be referenced elsewhere; still inline fields
              // into this sum prod so reduce builds the case class directly.
              val fieldSyms = c.fields.toList.map(f => ref(f.extracted))
              reduces += ((lab, idx) -> ReduceKind.Product(labelOf(c), fieldSyms.size))
              // Also ensure named NT exists for external refs (KeyPair.key: Str)
              ensureProduct(c)
              fieldSyms

            case c: ExtractedType.SumNonTerminal =>
              val childSym = ref(c)
              reduces += ((lab, idx) -> ReduceKind.Identity)
              List(childSym)

            case c: ExtractedType.SumTerminal =>
              val childSym = ref(c)
              reduces += ((lab, idx) -> ReduceKind.Identity)
              List(childSym)

            case c: ExtractedType.SumElement =>
              val childSym = ref(c)
              reduces += ((lab, idx) -> ReduceKind.Identity)
              List(childSym)

            case other =>
              report.errorAndAbort(s"Unsupported sum child in $lab: $other")
          }
        }

      NonEmptyList.fromList(prods) match {
        case Some(nel) => groups += NTGroup.BasicNT(GSym.Nt(lab), nel)
        case None      => report.errorAndAbort(s"Sum with no cases: $lab")
      }
    }
  }

}
