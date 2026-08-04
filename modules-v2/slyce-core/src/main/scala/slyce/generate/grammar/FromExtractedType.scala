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
        report.errorAndAbort(s"Parser root must be a non-terminal, got: $other")
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
          ensureSum(t)
          GSym.Nt(lab)

        case t: ExtractedType.SumTerminal =>
          report.errorAndAbort(s"SumTerminal not supported as production element yet: ${labelOf(t)}")

        case t: ExtractedType.SumElement =>
          report.errorAndAbort(s"SumElement not supported as production element yet: ${labelOf(t)}")

        case t: ExtractedType.ElementListBuiltIn =>
          listSym(t.elem, nonempty = false)

        case t: ExtractedType.NonEmptyElementListBuiltIn =>
          listSym(t.elem, nonempty = true)

        case t: ExtractedType.ElementOptionBuiltIn =>
          optSym(t.elem)

        case t: ExtractedType.IgnoreBuiltIn =>
          report.errorAndAbort("IgnoreBuiltIn fields are not supported in calculator path yet")

        case t: ExtractedType.UnionBuiltIn =>
          report.errorAndAbort(s"UnionBuiltIn not supported yet: ${t.typeRepr.showCode}")

        case t: ExtractedType.VElementListBuiltIn =>
          report.errorAndAbort("VElementList not supported in calculator path yet")

        case t: ExtractedType.NonEmptyVElementListBuiltIn =>
          report.errorAndAbort("NonEmptyVElementList not supported in calculator path yet")

        case other =>
          report.errorAndAbort(s"Unsupported ExtractedType: $other")
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
          // Head: only cons; Tail: cons | nil
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

    private def ensureSum(t: ExtractedType.SumNonTerminal): Unit = {
      val lab = labelOf(t)
      if seenNt.contains(lab) then return
      seenNt += lab

      // One production per direct child case. Product cases are inlined as field sequences
      // and reduced with that product's constructor (not a separate NT unless referenced elsewhere).
      val prods: List[List[GSym]] =
        t.directChildren.toList.zipWithIndex.map {
          case (child: ExtractedType.ProductNonTerminal, idx) =>
            products.update(labelOf(child), child)
            val fieldSyms = child.fields.toList.map(f => ref(f.extracted))
            reduces += ((lab, idx) -> ReduceKind.Product(labelOf(child), fieldSyms.size))
            fieldSyms
          case (child: ExtractedType.SumNonTerminal, idx) =>
            val childSym = ref(child) // recurse
            // lift: single child NT — treat as product arity 1 wrapping? Sum-of-sum:
            // production is just the child NT; value is already the child type which <: parent.
            // For Expr.Add vs nested, children are products only in calculator.
            reduces += ((lab, idx) -> ReduceKind.Product(labelOf(child), 1))
            List(childSym)
          case (child, _) =>
            report.errorAndAbort(s"Unsupported sum child in ${lab}: ${child}")
        }

      NonEmptyList.fromList(prods) match {
        case Some(nel) => groups += NTGroup.BasicNT(GSym.Nt(lab), nel)
        case None      => report.errorAndAbort(s"Sum with no cases: $lab")
      }
    }
  }

}
