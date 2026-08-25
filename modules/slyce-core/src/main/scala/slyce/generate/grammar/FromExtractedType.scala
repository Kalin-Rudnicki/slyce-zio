package slyce.generate.grammar

import java.util.UUID
import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.collection.mutable
import scala.quoted.*

import slyce.generate.*

/** Bridge ExtractedType tree → ExpandedGrammar. Also records reduce metadata for macro codegen (product/list/opt construction).
  */
private[slyce] object FromExtractedType {

  /** How to obtain one product field when reducing a product production. Stack indices refer to the RHS of the (possibly rewritten) production.
    */
  enum FieldSource {

    /** Use stack value at `idx` as this field. */
    case Arg(idx: Int)

    /** Stack value at `idx` is [[slyce.parse.FactoredSeq]]; use `.list`. */
    case FactoredList(idx: Int)

    /** Stack value at `idx` is [[slyce.parse.FactoredSeq]]; use `.trail`. */
    case FactoredTrail(idx: Int)

    /** Single-field product wrapper around `inner` (unit NT peeled during rewrite). */
    case Product1(typeLabel: String, inner: FieldSource)
  }

  enum ReduceKind {

    /** Case-class product: build fields from stack according to `sources` (one entry per case-class field, in declaration order).
      */
    case Product(typeLabel: String, sources: List[FieldSource])

    /** Production is a single child already of the desired type (e.g. sum of terminals). */
    case Identity
    case ListNil
    case ListCons
    case OptSome
    case OptNone

    /** `$seq.Head → ε` → empty FactoredSeq */
    case SeqEmpty

    /** `$seq.Head → T Tail` → combine shared term with FactoredTail */
    case SeqCons(elemProductLabel: String, restArity: Int)

    /** `$seq.Tail → ε` → FactoredTail.TrailOnly */
    case SeqTailTrail

    /** `$seq.Tail → rest… Head` → FactoredTail.Continues */
    case SeqTailMore(restArity: Int)
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
    private val foldedElemIds: mutable.Map[String, GSym] = mutable.Map.empty

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

    private def listSymOf(elem: GSym, nonempty: Boolean): GSym = {
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

    private def listSym(elemEt: ExtractedType, nonempty: Boolean): GSym = listSymOf(ref(elemEt), nonempty)

    private def optSymOf(elem: GSym): GSym = {
      val name = GSym.OptNt(elem.label)
      if !seenNt.contains(name.label) then {
        seenNt += name.label
        groups += NTGroup.Optional(elem)
        reduces += ((name.label, 0) -> ReduceKind.OptSome)
        reduces += ((name.label, 1) -> ReduceKind.OptNone)
      }
      name
    }

    private def optSym(elemEt: ExtractedType): GSym = optSymOf(ref(elemEt))

    /**
      * A synthetic "element followed by an ignore run" NT: `foldElem → elem T*` (or `elem T?` when
      * `bounded`), reducing (Identity) to the element value (the trailing ignore is discarded). Wrapping
      * THIS in the ordinary Opt/List machinery yields `(elem T*)?` / `(elem T*)*` — the ignore is folded
      * INSIDE the nullable, so an absent element contributes zero ignore runs and no two ignore runs are
      * ever adjacent. `bounded` picks `T?` (single maximal-munch ignore) over `T*`; the cache key already
      * distinguishes them because the opt- and list-NT labels differ.
      */
    private def foldedElemSym(elemEt: ExtractedType, igEt: ExtractedType, bounded: Boolean): GSym = {
      val elem = ref(elemEt)
      val ign = if bounded then optSym(igEt) else listSym(igEt, nonempty = false)
      val key = s"${elem.label}~${ign.label}"
      foldedElemIds.getOrElseUpdate(
        key, {
          val name = GSym.Nt(s"$$foldElem[$key]")
          groups += NTGroup.BasicNT(name, NonEmptyList.one(List(elem, ign)))
          reduces += ((name.label, 0) -> ReduceKind.Identity)
          name
        },
      )
    }

    /** One declared ignore slot: the ignore terminal(s) plus whether the slot is bounded (`T?`) or star (`T*`). */
    private final case class Ign(et: ExtractedType, bounded: Boolean)

    /** Ignore terminals declared on a product via `@ignoreBefore/@ignoreBetween/@ignoreAfter[T]` (star) or their `…One` (bounded) variants. */
    private final case class IgnoreSpec(
        before: Option[Ign],
        between: Option[Ign],
        after: Option[Ign],
    ) {
      def isEmpty: Boolean = before.isEmpty && between.isEmpty && after.isEmpty
    }

    private val ignoreFqns: Set[String] =
      Set(
        "slyce.parse.ignoreBefore", "slyce.parse.ignoreBetween", "slyce.parse.ignoreAfter",
        "slyce.parse.ignoreBeforeOne", "slyce.parse.ignoreBetweenOne", "slyce.parse.ignoreAfterOne",
      )

    /**
      * Fail loud on a FIELD-level ignore annotation. Only PRODUCT-level `@ignore*` is implemented
      * (design "Option A"); a field-level annotation (design "Option C", per-list ignore) would
      * otherwise be silently ignored — a footgun. Erroring is better than a silent no-op.
      */
    private def assertNoFieldLevelIgnore(gen: ProductGeneric.CaseClassGeneric[?]): Unit =
      gen.fields.foreach { f =>
        f.annotations.all.map(_.tpe.typeSymbol.fullName).find(ignoreFqns.contains).foreach { fqn =>
          report.errorAndAbort(
            s"@${fqn.split('.').last} on field '${f.name}' is not supported — put ignore annotations on the " +
              s"PRODUCT (the case class), not a field. Field-level (per-list) ignore is not implemented yet.",
            f.pos,
          )
        }
      }

    private def readIgnores(gen: ProductGeneric.CaseClassGeneric[?]): IgnoreSpec = {
      assertNoFieldLevelIgnore(gen)
      val annTypes: List[TypeRepr] = gen.annotations.all.map(_.tpe)
      def find(fqn: String): Option[ExtractedType] =
        annTypes
          .collectFirst { case tr if tr.typeSymbol.fullName == fqn && tr.typeArgs.nonEmpty => tr.typeArgs.head }
          .map(tr => cache.getOrCreate(gen.pos)(tr))
      def slot(starFqn: String, oneFqn: String): Option[Ign] =
        (find(starFqn), find(oneFqn)) match {
          case (Some(_), Some(_)) =>
            report.errorAndAbort(
              s"a product may not carry both @${starFqn.split('.').last} and @${oneFqn.split('.').last}",
              gen.pos,
            )
          case (_, Some(et)) => Some(Ign(et, bounded = true))
          case (Some(et), _) => Some(Ign(et, bounded = false))
          case (None, None)  => None
        }
      IgnoreSpec(
        before = slot("slyce.parse.ignoreBefore", "slyce.parse.ignoreBeforeOne"),
        between = slot("slyce.parse.ignoreBetween", "slyce.parse.ignoreBetweenOne"),
        after = slot("slyce.parse.ignoreAfter", "slyce.parse.ignoreAfterOne"),
      )
    }

    /**
      * Interleave ignore into a product's RHS using the **trailing-fold** rule, so nullable fields
      * (Option/List) never create two adjacent ignore runs:
      *   - `@ignoreBefore` → one leading `T*`.
      *   - each field carries its TRAILING ignore (`@ignoreBetween` on non-last fields, `@ignoreAfter`
      *     on the last field). For a NON-nullable field that is `field  T*` (two RHS symbols, the `T*`
      *     discarded); for a nullable `ElementOption[B]` / `ElementList[B]` / `NonEmptyElementList[B]`
      *     the trailing ignore is folded INSIDE via [[foldedElemSym]] → `(B T*)?` / `(B T*)*` / `(B T*)+`,
      *     so an absent element contributes no ignore run and the previous field's trailing run covers
      *     the gap. Field sources point at each real field's slot; ignore slots are discarded.
      */
    private def productRhs(gen: ProductGeneric.CaseClassGeneric[?], fields: List[ExtractedType]): (List[GSym], List[FieldSource]) = {
      val ign = readIgnores(gen)
      if ign.isEmpty then {
        val syms = fields.map(ref)
        (syms, syms.indices.map(FieldSource.Arg(_)).toList)
      } else {
        val rhs = mutable.ArrayBuffer.empty[GSym]
        val sources = mutable.ArrayBuffer.empty[FieldSource]
        def ignSlot(b: Ign): GSym = if b.bounded then optSym(b.et) else listSym(b.et, nonempty = false)
        ign.before.foreach(b => rhs += ignSlot(b))
        val n = fields.size
        fields.zipWithIndex.foreach { case (fEt, i) =>
          val trailing: Option[Ign] = if i == n - 1 then ign.after else ign.between
          trailing match {
            case None =>
              sources += FieldSource.Arg(rhs.size)
              rhs += ref(fEt)
            case Some(b) =>
              sources += FieldSource.Arg(rhs.size)
              fEt match {
                case o: ExtractedType.ElementOptionBuiltIn        => rhs += optSymOf(foldedElemSym(o.elem, b.et, b.bounded))
                case l: ExtractedType.ElementListBuiltIn          => rhs += listSymOf(foldedElemSym(l.elem, b.et, b.bounded), nonempty = false)
                case l: ExtractedType.NonEmptyElementListBuiltIn  => rhs += listSymOf(foldedElemSym(l.elem, b.et, b.bounded), nonempty = true)
                case _                                            =>
                  rhs += ref(fEt)
                  rhs += ignSlot(b)
              }
          }
        }
        (rhs.toList, sources.toList)
      }
    }

    private def ensureProduct(t: ExtractedType.ProductNonTerminal): Unit = {
      val lab = labelOf(t)
      if seenNt.contains(lab) then return
      seenNt += lab
      products.update(lab, t)

      val (rhs, sources) = productRhs(t.gen, t.fields.toList.map(_.extracted))
      groups += NTGroup.BasicNT(GSym.Nt(lab), NonEmptyList.one(rhs))
      reduces += ((lab, 0) -> ReduceKind.Product(lab, sources))
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

    /** One production per direct child:
      *   - ProductTerminal → [Term] + Identity
      *   - ProductNonTerminal → inlined fields + Product reduce
      *   - nested Sum* → [ChildNt] + Identity (value already has parent type if <: Element)
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
              // into this sum prod so reduce builds the case class directly. Ignore slots
              // (@ignoreBefore/Between/After) are interleaved the same way as a standalone product.
              val (rhs, sources) = productRhs(c.gen, c.fields.toList.map(_.extracted))
              reduces += ((lab, idx) -> ReduceKind.Product(labelOf(c), sources))
              // Also ensure named NT exists for external refs (KeyPair.key: Str)
              ensureProduct(c)
              rhs

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
