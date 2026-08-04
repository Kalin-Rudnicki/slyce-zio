package slyce.generate.grammar

import java.util.UUID
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.collection.mutable
import scala.quoted.*

/**
 * Grammar-level rewrite pass (no Scala AST / field-shape hardcoding).
 *
 * Operates on [[FromExtractedType.Result]] after extraction:
 *   1. Compute FIRST / nullable on the expanded NT graph
 *   2. Peel unit nonterminals (single prod, single symbol) — so Ex2-style nesting is visible
 *   3. When adjacent symbols conflict on FIRST and the left is a nullable list whose element
 *      is uniquely `T · rest…` (rest non-empty) and the right peels to `Opt[T]`, left-factor
 *      into `$seq` and record product [[FromExtractedType.FieldSource]] covers
 *   4. Refuse inherently ambiguous consecutive same-FIRST lists with a hard error
 *
 * See `GRAMMAR-REWRITE-CONSTRAINTS.md`.
 */
private[slyce] object GrammarRewrite {

  def apply(result: FromExtractedType.Result)(using Quotes): FromExtractedType.Result = {
    val ctx = new Ctx(result)
    ctx.rewriteAll()
    ctx.result
  }

  private final class Ctx(initial: FromExtractedType.Result)(using Quotes) {
    private var groups: List[NTGroup] = initial.grammar.ntGroups
    private val reduces: mutable.Map[(String, Int), FromExtractedType.ReduceKind] =
      mutable.Map.from(initial.reduces)
    private val products = initial.products
    private val terminals = initial.terminals
    private val startNt = initial.grammar.startNt
    private val maxLookAhead = initial.grammar.maxLookAhead

    def result: FromExtractedType.Result =
      FromExtractedType.Result(
        grammar = ExpandedGrammar(startNt, maxLookAhead, groups),
        reduces = reduces.toMap,
        terminals = terminals,
        products = products,
      )

    /** Map NT label → productions (as symbol lists). Only BasicNT. */
    private def basicProds: Map[String, List[List[GSym]]] =
      groups.collect { case NTGroup.BasicNT(GSym.Nt(lab), prods) =>
        lab -> prods.toList
      }.toMap

    private def listElem: Map[String, GSym] =
      groups.collect { case NTGroup.ListNT(id, elem, _) => id -> elem }.toMap

    private def listNonempty: Map[String, Boolean] =
      groups.collect { case NTGroup.ListNT(id, _, ne) => id -> ne }.toMap

    private def optChild: Map[String, GSym] =
      groups.collect { case NTGroup.Optional(child) =>
        GSym.OptNt(child.label).label -> child
      }.toMap

    private def productionsOf(nt: GSym.NonTerm): List[List[GSym]] =
      nt match {
        case GSym.Nt(lab) =>
          basicProds.getOrElse(lab, Nil)
        case GSym.ListNt(id, phase) =>
          listElem.get(id) match {
            case None => Nil
            case Some(elem) =>
              val ne = listNonempty.getOrElse(id, false)
              if ne then
                phase match {
                  case GSym.ListPhase.Head => List(List(elem, GSym.ListNt(id, GSym.ListPhase.Tail)))
                  case GSym.ListPhase.Tail =>
                    List(List(elem, GSym.ListNt(id, GSym.ListPhase.Tail)), Nil)
                  case GSym.ListPhase.Simple => Nil
                }
              else
                phase match {
                  case GSym.ListPhase.Simple => List(List(elem, GSym.ListNt(id, phase)), Nil)
                  case _                     => Nil
                }
          }
        case GSym.OptNt(childLab) =>
          optChild.get(GSym.OptNt(childLab).label) match {
            case Some(c) => List(List(c), Nil)
            case None    => Nil
          }
        case GSym.SeqNt(id, phase) =>
          groups.collectFirst { case NTGroup.LeftFactoredSeq(`id`, t, rest) =>
            phase match {
              case GSym.SeqPhase.Head => List(List(t, GSym.SeqNt(id, GSym.SeqPhase.Tail)), Nil)
              case GSym.SeqPhase.Tail => List(rest :+ GSym.SeqNt(id, GSym.SeqPhase.Head), Nil)
            }
          }.getOrElse(Nil)
      }

    private def allNonTerms: Set[GSym.NonTerm] =
      groups.flatMap(_.rawNTs.toList.map(_.name)).toSet

    private def firstAndNullable: (Map[GSym, Set[GSym.Term]], Map[GSym, Boolean]) = {
      val first = mutable.Map.empty[GSym, Set[GSym.Term]]
      val nullable = mutable.Map.empty[GSym, Boolean]

      def ensure(s: GSym): Unit =
        if !first.contains(s) then {
          first(s) = Set.empty
          nullable(s) = false
        }

      allNonTerms.foreach(ensure)
      groups.foreach { g =>
        g.rawNTs.toList.foreach { nt =>
          nt.productions.toList.foreach { p =>
            p.elements.foreach {
              case t: GSym.Term =>
                first(t) = Set(t)
                nullable(t) = false
              case n: GSym.NonTerm => ensure(n)
            }
          }
        }
      }

      var changed = true
      while changed do {
        changed = false
        allNonTerms.foreach { nt =>
          val prods = productionsOf(nt)
          var ntNull = false
          var ntFirst = Set.empty[GSym.Term]
          prods.foreach { elems =>
            var prefixNull = true
            elems.foreach { sym =>
              if prefixNull then {
                ensure(sym)
                ntFirst ++= first.getOrElse(sym, Set.empty)
                prefixNull = nullable.getOrElse(sym, false)
              }
            }
            if prefixNull then ntNull = true
          }
          if ntFirst != first.getOrElse(nt, Set.empty) || ntNull != nullable.getOrElse(nt, false) then {
            first(nt) = ntFirst
            nullable(nt) = ntNull
            changed = true
          }
        }
      }
      (first.toMap, nullable.toMap)
    }

    /**
     * Peel through unit NTs: single production of a single symbol.
     * `wraps` are outer product NTs (labels) from outside in.
     */
    private final case class Peeled(core: GSym, wraps: List[String])

    private def peel(sym: GSym, seen: Set[String] = Set.empty): Peeled =
      sym match {
        case nt: GSym.Nt if !seen.contains(nt.label) =>
          basicProds.get(nt.label) match {
            case Some(List(List(inner))) =>
              val innerP = peel(inner, seen + nt.label)
              Peeled(innerP.core, nt.label :: innerP.wraps)
            case _ => Peeled(nt, Nil)
          }
        case other => Peeled(other, Nil)
      }

    private def firstOf(sym: GSym, first: Map[GSym, Set[GSym.Term]]): Set[GSym.Term] =
      first.getOrElse(sym, sym match {
        case t: GSym.Term => Set(t)
        case _            => Set.empty
      })

    private def isNullable(sym: GSym, nullable: Map[GSym, Boolean]): Boolean =
      nullable.getOrElse(sym, false)

    /** Unique production of a named product NT, if any. */
    private def uniqueProd(nt: GSym.Nt): Option[List[GSym]] =
      basicProds.get(nt.label) match {
        case Some(List(only)) => Some(only)
        case _                => None
      }

    /**
     * If list element is a product `T · rest…` with rest non-empty and T matching opt child term,
     * return (sharedTerm, rest, elemProductLabel).
     */
    private def leftFactorShape(
        listCore: GSym.ListNt,
        optCore: GSym.OptNt,
    ): Option[(GSym.Term, List[GSym], String)] = {
      if listNonempty.getOrElse(listCore.id, false) then None
      else if listCore.phase != GSym.ListPhase.Simple then None
      else
        (listElem.get(listCore.id), optChild.get(optCore.label)) match {
          case (Some(elem), Some(child: GSym.Term)) =>
            elem match {
              case nt: GSym.Nt =>
                uniqueProd(nt) match {
                  case Some(hd :: rest) if rest.nonEmpty =>
                    hd match {
                      case t: GSym.Term if t.label == child.label =>
                        Some((child, rest, nt.label))
                      case _ => None
                    }
                  case _ => None
                }
              case _ => None
            }
          case _ => None
        }
    }

    private def applyWraps(wraps: List[String], core: FromExtractedType.FieldSource): FromExtractedType.FieldSource =
      wraps.foldRight(core) { (lab, inner) =>
        FromExtractedType.FieldSource.Product1(lab, inner)
      }

    def rewriteAll(): Unit = {
      var guard = 0
      var progress = true
      while progress && guard < 64 do {
        guard += 1
        progress = false
        val (first, nullable) = firstAndNullable
        // snapshot groups length for iteration
        val basic = groups.zipWithIndex.collect { case (g: NTGroup.BasicNT, idx) => (idx, g) }
        basic.foreach { case (gIdx, NTGroup.BasicNT(GSym.Nt(prodLab), prodsNel)) =>
          val prods = prodsNel.toList
          prods.zipWithIndex.foreach { case (elems, pIdx) =>
            var i = 0
            while i < elems.length - 1 do {
              val left = elems(i)
              val right = elems(i + 1)
              val pl = peel(left)
              val pr = peel(right)
              val fL = firstOf(pl.core, first)
              val fR = firstOf(pr.core, first)
              val overlap = fL.intersect(fR)

              if overlap.nonEmpty then {
                (pl.core, pr.core) match {
                  case (l: GSym.ListNt, r: GSym.ListNt) =>
                    val lw = if pl.wraps.isEmpty then "∅" else pl.wraps.mkString(" → ")
                    val rw = if pr.wraps.isEmpty then "∅" else pr.wraps.mkString(" → ")
                    report.errorAndAbort(
                      s"""|Invalid grammar (inherently ambiguous adjacent lists after peel):
                          |  while rewriting production of $prodLab
                          |  left:  ${pl.core} (wraps: $lw)
                          |  right: ${pr.core} (wraps: $rw)
                          |  overlapping FIRST terminals: ${overlap.map(_.label).mkString(", ")}
                          |  Consecutive lists with overlapping FIRST cannot be uniquely split.
                          |""".stripMargin,
                    )

                  case (l: GSym.ListNt, r: GSym.OptNt) if isNullable(pl.core, nullable) || l.phase == GSym.ListPhase.Simple =>
                    leftFactorShape(l, r) match {
                      case Some((shared, rest, elemLab)) =>
                        val seqId = UUID.randomUUID().toString
                        val headSym = GSym.SeqNt(seqId, GSym.SeqPhase.Head)
                        groups = groups :+ NTGroup.LeftFactoredSeq(seqId, shared, rest)
                        reduces += ((headSym.label, 0) -> FromExtractedType.ReduceKind.SeqCons(elemLab, rest.size))
                        reduces += ((headSym.label, 1) -> FromExtractedType.ReduceKind.SeqEmpty)
                        reduces += ((GSym.SeqNt(seqId, GSym.SeqPhase.Tail).label, 0) -> FromExtractedType.ReduceKind.SeqTailMore(rest.size))
                        reduces += ((GSym.SeqNt(seqId, GSym.SeqPhase.Tail).label, 1) -> FromExtractedType.ReduceKind.SeqTailTrail)

                        val newElems = elems.take(i) ++ List(headSym) ++ elems.drop(i + 2)
                        val newProds = prods.updated(pIdx, newElems)
                        NonEmptyList.fromList(newProds) match {
                          case Some(nel) =>
                            groups = groups.updated(gIdx, NTGroup.BasicNT(GSym.Nt(prodLab), nel))
                          case None =>
                            report.errorAndAbort(s"Internal rewrite error: empty prods for $prodLab")
                        }

                        // Update product reduce field sources if this is a product NT
                        products.get(prodLab).foreach { _ =>
                          val oldKind = reduces.get((prodLab, pIdx))
                          oldKind match {
                            case Some(FromExtractedType.ReduceKind.Product(tl, sources)) =>
                              // Old sources are Arg(0)..Arg(n-1) aligned with *pre-rewrite* elems.
                              // Two consecutive stack slots i, i+1 become one slot i (seq).
                              val newSources = rebuildSources(sources, i, pl.wraps, pr.wraps)
                              reduces((prodLab, pIdx)) = FromExtractedType.ReduceKind.Product(tl, newSources)
                            case _ =>
                              // Sum-inlined product reduce: sources are for child product, stack is fields not list/opt
                              ()
                          }
                        }

                        // Also handle sum productions that inline product fields — rare for list+opt
                        // For BasicNT product with Product reduce only.

                        progress = true
                        i = elems.length // break inner; restart outer while
                      case None =>
                        // Overlap but not our left-factor shape — leave for table / later error
                        i += 1
                    }

                  case _ =>
                    i += 1
                }
              } else i += 1
            }
          }
        }
      }
    }

    /**
     * Rebuild field sources after merging stack slots `i` and `i+1` into a FactoredSeq at `i`.
     * Pre-rewrite sources are typically Arg(0)..Arg(n-1).
     * Left wraps apply to FactoredList; right wraps to FactoredTrail.
     */
    private def rebuildSources(
        sources: List[FromExtractedType.FieldSource],
        mergeIdx: Int,
        leftWraps: List[String],
        rightWraps: List[String],
    ): List[FromExtractedType.FieldSource] = {
      // Map old stack index → new stack index (indices > mergeIdx+1 shift down by 1;
      // mergeIdx and mergeIdx+1 both map to mergeIdx)
      def mapIdx(old: Int): Int =
        if old <= mergeIdx then old
        else if old == mergeIdx + 1 then mergeIdx
        else old - 1

      def rewriteSrc(src: FromExtractedType.FieldSource): FromExtractedType.FieldSource =
        src match {
          case FromExtractedType.FieldSource.Arg(j) if j == mergeIdx =>
            applyWraps(leftWraps, FromExtractedType.FieldSource.FactoredList(mergeIdx))
          case FromExtractedType.FieldSource.Arg(j) if j == mergeIdx + 1 =>
            applyWraps(rightWraps, FromExtractedType.FieldSource.FactoredTrail(mergeIdx))
          case FromExtractedType.FieldSource.Arg(j) =>
            FromExtractedType.FieldSource.Arg(mapIdx(j))
          case FromExtractedType.FieldSource.FactoredList(j) =>
            FromExtractedType.FieldSource.FactoredList(mapIdx(j))
          case FromExtractedType.FieldSource.FactoredTrail(j) =>
            FromExtractedType.FieldSource.FactoredTrail(mapIdx(j))
          case FromExtractedType.FieldSource.Product1(lab, inner) =>
            FromExtractedType.FieldSource.Product1(lab, rewriteSrc(inner))
        }

      sources.map(rewriteSrc)
    }
  }

}
