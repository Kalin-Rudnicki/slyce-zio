package slyce.generate.grammar

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.list.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import klib.utils.{given, *}
import scala.annotation.tailrec

import slyce.core.*
import slyce.generate.*

final case class ParsingTable private (
    // TODO (KR) :
)
object ParsingTable {

  // TODO (KR) : Make configurable
  val MaxLookAhead: Int = 3

  object fromExpandedGrammar {

    /*
       What do we want here?
       Effectively something along the lines of a List[State].
       What is a State?
       Map[Input, Action].
     */

    def apply(expandedGrammar: ExpandedGrammar): Validated[ParsingTable] =
      Validated.withValidations(
        validateDefinedNTs(expandedGrammar.startNt, expandedGrammar.deDuplicatedNTGroups),
      ) {
        val ntMap: Map[ExpandedGrammar.Identifier.NonTerminal, NonEmptyList[ExpandedGrammar.Production]] =
          expandedGrammar.deDuplicatedNTGroups.flatMap(_.rawNTs.toList.map(nt => (nt.name, nt.productions))).toMap

        val initialEntries: Set[ExpandedEntry] =
          Set(
            ExpandedEntry(
              reducesTo = ReducesTo.###,
              seen = Nil,
              waiting = ExpandedGrammar.Identifier.NonTerminal.NamedNt(expandedGrammar.startNt.value) :: Nil,
              lookAhead = Follow(Set.empty, true) :: Nil,
            ),
          )

        val expanded: Set[ExpandedEntry] = expandEntries(ntMap, initialEntries)

        // TODO (KR) :
        Validated.???
      }

    // =====| Types |=====

    private final case class ExpandedEntry(
        reducesTo: ReducesTo,
        seen: List[ExpandedGrammar.Identifier],
        waiting: List[ExpandedGrammar.Identifier],
        lookAhead: List[Follow],
    )

    private final case class Follow(
        validTerminals: Set[ExpandedGrammar.Identifier.Term],
        eofIsValid: Boolean,
    ) {
      override def toString: String =
        (validTerminals.map(_.toString).toList.sorted ::: Option.when(eofIsValid)("$".cyan.toString).toList).mkString("Follow< ", ", ", " >")
    }

    private enum ReducesTo {
      case ###
      case Production(nt: ExpandedGrammar.Identifier.NonTerminal, idx: Int)

      override final def toString: String =
        this match {
          case ReducesTo.###                 => "###"
          case ReducesTo.Production(nt, idx) => s"$nt[$idx]"
        }

    }

    // =====| Helpers |=====

    // TODO (KR) : Remove
    private object debugging {

      def showEntries(label: String, entries: Set[ExpandedEntry]): Unit = {
        println()
        println()
        println()
        println(s"=====| $label (${entries.size}) |=====")
        println {
          entries.toList
            .sortBy(_.toString)
            .map { e =>
              IndentedString.inline(
                s">> ${e.reducesTo}",
                IndentedString.indented(
                  s"     seen[${e.seen.size}]: ${e.seen.mkString(" , ".red.toString)}",
                  s"  waiting[${e.waiting.size}]: ${e.waiting.mkString(" , ".red.toString)}",
                  s"lookAhead[${e.lookAhead.size}]: ${e.lookAhead.mkString(" , ".red.toString)}",
                ),
              )
            }
            .toString("  ")
        }
      }

    }

    private def validateDefinedNTs(startName: Marked[String], ntgs: List[ExpandedGrammar.NTGroup]): Validated[Any] = {
      val allDefinedNTNames: List[ExpandedGrammar.Identifier.NonTerminal] =
        ntgs.flatMap(_.rawNTs.toList).map(_.name)
      val allReferencedNTs: Set[ExpandedGrammar.Identifier.NonTerminal] =
        ntgs
          .flatMap(_.rawNTs.toList.flatMap(_.productions.toList.flatMap(_.elements)))
          .collect { case nt: ExpandedGrammar.Identifier.NonTerminal => nt }
          .toSet
      val allDefinedNTNameSet: Set[ExpandedGrammar.Identifier.NonTerminal] =
        allDefinedNTNames.toSet

      // TODO (KR) : Validate 'terminals' as well?

      Validated.withValidations(
        allDefinedNTNames.groupBy(identity).toList.parTraverse { (nt, nts) =>
          if (nts.size == 1) ().asRight
          else Marked(s"NT defined multiple times: $nt", Span.Unknown).leftNel
        },
        allReferencedNTs.toList.parTraverse { nt =>
          if (allDefinedNTNameSet.contains(nt)) ().asRight
          else Marked(s"NT is referenced but not defined: $nt", Span.Unknown).leftNel
        },
        if (allDefinedNTNameSet.contains(ExpandedGrammar.Identifier.NonTerminal.NamedNt(startName.value))) ().asRight
        else startName.as(s"StartMode references undefined NT: ${startName.value}").leftNel,
      ) { ().asRight }
    }

    private def mergeFollows(follows: List[List[Follow]]): List[Follow] =
      follows.flatMap(_.toNel).toNel match {
        case Some(nels) =>
          val heads = nels.map(_.head)
          val tails = nels.toList.map(_.tail)
          Follow(heads.toList.toSet.flatMap(_.validTerminals), heads.exists(_.eofIsValid)) :: mergeFollows(tails)
        case None =>
          Nil
      }

    private def calcLookAhead(
        ntMap: Map[ExpandedGrammar.Identifier.NonTerminal, NonEmptyList[ExpandedGrammar.Production]],
        ids: List[(ReducesTo, Int, ExpandedGrammar.Identifier)],
        alreadyExpanded: Set[(ReducesTo, Int)],
        ifPassThrough: List[Follow],
        maxLookAhead: Int,
    ): List[Follow] =
      if (maxLookAhead <= 0) Nil
      else
        ids match {
          case Nil => ifPassThrough.take(maxLookAhead)
          case (rt, sc, id) :: tail =>
            id match {
              case nt: ExpandedGrammar.Identifier.NonTerminal =>
                if (alreadyExpanded.contains((rt, sc))) Nil
                else {
                  val newExpanded: Set[(ReducesTo, Int)] = alreadyExpanded + (rt -> sc)
                  val inlined: List[List[(ReducesTo, Int, ExpandedGrammar.Identifier)]] =
                    ntMap(nt).toList.zipWithIndex.map { (prod, idx) =>
                      val rt = ReducesTo.Production(nt, idx)
                      prod.elements.zipWithIndex.map { (i, idx) => (rt, idx, i) }
                    }
                  mergeFollows(inlined.map(ids => calcLookAhead(ntMap, ids ::: tail, newExpanded, ifPassThrough, maxLookAhead)))
                }
              case t: ExpandedGrammar.Identifier.Term =>
                Follow(Set(t), false) :: calcLookAhead(ntMap, tail, Set.empty, ifPassThrough, maxLookAhead - 1)
            }
        }

    private def expandEntries(
        ntMap: Map[ExpandedGrammar.Identifier.NonTerminal, NonEmptyList[ExpandedGrammar.Production]],
        initial: Set[ExpandedEntry],
    ): Set[ExpandedEntry] = {
      val preJoinedExpansion: Set[ExpandedEntry] =
        Helpers.findAll(initial) {
          case ExpandedEntry(rt, seen, (next: ExpandedGrammar.Identifier.NonTerminal) :: waiting, lookAhead) =>
            val lookup: NonEmptyList[ExpandedGrammar.Production] = ntMap(next)

            val newIds: List[(ReducesTo, Int, ExpandedGrammar.Identifier)] =
              waiting.zipWithIndex.map { (id, idx) => (rt, seen.size + 1 + idx, id) }

            val newFollows: List[Follow] =
              calcLookAhead(ntMap, newIds, Set.empty, lookAhead, MaxLookAhead)

            lookup.toList.zipWithIndex.map { (prod, idx) =>
              ExpandedEntry(
                reducesTo = ReducesTo.Production(next, idx),
                seen = Nil,
                waiting = prod.elements,
                lookAhead = newFollows,
              )
            }.toSet
          case _ =>
            Set.empty
        }

      val joinedExpansion: Set[ExpandedEntry] =
        preJoinedExpansion.groupMap(e => (e.reducesTo, e.seen, e.waiting))(_.lookAhead).toSet.map { case ((rt, seen, waiting), follows) =>
          ExpandedEntry(rt, seen, waiting, mergeFollows(follows.toList))
        }

      debugging.showEntries("initial", initial)
      debugging.showEntries("preJoinedExpansion", preJoinedExpansion)
      debugging.showEntries("joinedExpansion", joinedExpansion)

      joinedExpansion
    }

  }
}
