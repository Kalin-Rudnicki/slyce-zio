package slyce.generate.grammar

import cats.data.NonEmptyList
import cats.syntax.either.*
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
  val MaxLookAhead: Int = 1

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
        // TODO (KR) :
        // expandClosures(expandedGrammar.deDuplicatedNTGroups)
        todoRename(expandedGrammar.startNt.value, expandedGrammar.deDuplicatedNTGroups)

        // TODO (KR) :
        Validated.???
      }

    // =====| Types |=====

    final case class Closure(entries: Set[Entry])

    final case class Entry(
        reducesTo: ReducesTo,
        seen: List[ExpandedGrammar.Identifier],
        waiting: List[ExpandedGrammar.Identifier],
        lookAhead: LookAhead,
    )

    sealed trait LookAhead
    object LookAhead {
      case object $ extends LookAhead
      // TODO (KR) : '?'
      final case class Terminal(terminal: ExpandedGrammar.Identifier.Term, next: LookAhead) extends LookAhead {
        override def toString: String = s"$terminal :: $next"
      }
      final case class NonTerminal(nonTerminal: ExpandedGrammar.Identifier.NonTerminal, next: LookAhead) extends LookAhead {
        override def toString: String = s"$nonTerminal :: $next"
      }
    }

    enum ReducesTo {
      case ###
      case Production(nt: ExpandedGrammar.Identifier.NonTerminal, idx: Int)

      override final def toString: String =
        this match {
          case ReducesTo.###                 => "###"
          case ReducesTo.Production(nt, idx) => s"$nt[$idx]"
        }

    }

    // =====| Helpers |=====

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

    // TODO (KR) : Rename
    private def todoRename(startName: String, ntgs: List[ExpandedGrammar.NTGroup]): Unit = {
      val ntMap: Map[ExpandedGrammar.Identifier.NonTerminal, NonEmptyList[ExpandedGrammar.Production]] =
        ntgs.flatMap(_.rawNTs.toList.map(nt => (nt.name, nt.productions))).toMap

      val startingClosure =
        Closure(
          Set(
            Entry(
              reducesTo = ReducesTo.###,
              seen = Nil,
              waiting = ExpandedGrammar.Identifier.NonTerminal.NamedNt(startName) :: Nil,
              lookAhead = LookAhead.$,
            ),
          ),
        )

      def showEntries(label: String, entries: Set[Entry]): Unit = {
        val empty = "###"
        val maxReducesToLength: Int = entries.map(_.reducesTo.toString).map(_.length).maxOption.getOrElse(0)

        println()
        println(s"=====| $label [${entries.size}] |=====")
        entries.toList.sortBy(_.reducesTo.toString).foreach { e =>
          println(s"${e.reducesTo.toString.alignLeft(maxReducesToLength)} ${"->".green} ${e.seen.mkString("  ")} ${".".red} ${e.waiting.mkString("  ")} ${",".cyan} ${e.lookAhead}")
        }

      }

      @tailrec
      def buildLookahead(ids: List[ExpandedGrammar.Identifier], lookAhead: LookAhead): LookAhead =
        ids match {
          case head :: tail =>
            head match {
              case nt: ExpandedGrammar.Identifier.NonTerminal => buildLookahead(tail, LookAhead.NonTerminal(nt, lookAhead))
              case t: ExpandedGrammar.Identifier.Term         => buildLookahead(tail, LookAhead.Terminal(t, lookAhead))
            }
          case Nil => lookAhead
        }

      @tailrec
      def expandClosure(
          unseen: Set[Entry],
          seen: Set[Entry],
          derivedBy: Set[(ReducesTo, Int, ExpandedGrammar.Identifier.NonTerminal)],
          maxDepth: Int,
      ): Closure = {
        println()
        println()
        println(s"expandClosure($maxDepth):")
        showEntries("unseen", unseen)
        showEntries("seen", seen)
        derivedBy.toList.sortBy(_.toString).foreach { (rt, nt, size) =>
          println(s"  >> $rt, $nt, $size")
        }

        val toExpand: Set[(ReducesTo, ExpandedGrammar.Identifier.NonTerminal, Int, List[ExpandedGrammar.Identifier], LookAhead)] =
          unseen
            .collect { case Entry(rt, seen, (nt: ExpandedGrammar.Identifier.NonTerminal) :: next, lookAhead) => (rt, nt, seen.size, next, lookAhead) }
            .filterNot { (rt, nt, size, _, _) => derivedBy.contains((rt, size, nt)) }

        // TODO (KR) : Remove this check once I have more confidence this wont ever happen
        if (maxDepth <= 0) throw new RuntimeException("Internal Defect : Most likely encountered an infinite loop in ParsingTable.expandClosure")
        else if (toExpand.isEmpty) Closure(unseen | seen)
        else {
          val expanded: Set[Entry] =
            toExpand.flatMap { (rt, nt, _, next, lookahead) =>
              println()
              println(s"  > $rt, $nt")
              ntMap(nt).toList.zipWithIndex.map { (prod, idx) =>
                println(s"    > ${prod.elements.headOption}")
                Entry(
                  reducesTo = ReducesTo.Production(nt, idx),
                  seen = Nil,
                  waiting = prod.elements,
                  lookAhead = buildLookahead(next, lookahead),
                )
              }
            }

          // val filteredExpanded: Set[Entry] = ???

          expandClosure(
            expanded,
            seen | unseen,
            derivedBy | toExpand.map { (rt, nt, size, _, _) => (rt, size, nt) },
            maxDepth - 1,
          )
        }
      }

      val res = expandClosure(startingClosure.entries, Set.empty, Set.empty, 100)

      showEntries("result", res.entries)

      println()
      println()
      println()
      val (finished, notFinished) =
        res.entries.partitionMap {
          case entry @ Entry(_, _, Nil, _)                           => entry.asLeft
          case Entry(reducesTo, seen, next :: newWaiting, lookAhead) => (next, Entry(reducesTo, seen :+ next, newWaiting, lookAhead)).asRight
        }

      showEntries("finished", finished)
      notFinished.groupMap(_._1)(_._2).toList.sortBy(_._1.toString).foreach { (id, entries) => showEntries(id.toString, entries) }

      ()
    }

    // TODO (KR) : Remove?
    /*
    private def expandClosures(ntgs: List[ExpandedGrammar.NTGroup]): Map[ExpandedGrammar.Identifier.NonTerminal, Closure] = {
      val nonExpandedClosures: Map[ExpandedGrammar.Identifier.NonTerminal, Closure] =
        ntgs
          .flatMap(_.rawNTs.toList)
          .map { nt =>
            (
              nt.name,
              Closure(
                nt.productions.toList.map { prod =>
                  Closure.Entry(
                    nt.name,
                    Nil,
                    prod.elements,
                  )
                }.toSet,
              ),
            )
          }
          .toMap

      // TODO (KR) : What if nt has empty production

      val expandedClosures: Map[ExpandedGrammar.Identifier.NonTerminal, Closure] =
        nonExpandedClosures.map { (nt, closure) =>
          (
            nt,
            Closure(
              Helpers.findAll(closure.entries) { e =>
                e.waiting.headOption
                  .collect { case nt: ExpandedGrammar.Identifier.NonTerminal => nt }
                  .fold(Set.empty[Closure.Entry])(nonExpandedClosures(_).entries)
              },
            ),
          )
        }

      def show(label: String, map: Map[ExpandedGrammar.Identifier.NonTerminal, Closure]): Unit = {
        println()
        println(s"=====| $label [${map.toList.flatMap(_._2.entries).size}] |=====")
        map.toList.sortBy(_._1.toString).foreach { (nt, closure) =>
          println(s"--- $nt [${closure.entries.size}] ---")
          closure.entries.toList.sortBy(_.toString).foreach { entry =>
            println(s"  - ${entry.reducesTo} : ${entry.seen.mkString(" ")} . ${entry.waiting.mkString(" ")}")
          }
        }
      }

      show("nonExpanded", nonExpandedClosures)
      show("expanded", expandedClosures)

      expandedClosures
    }
     */

  }
}
