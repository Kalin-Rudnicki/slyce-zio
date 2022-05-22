package slyce.generate.grammar

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.parallel.*

import slyce.core.*
import slyce.generate.*

final case class ParsingTable private (
    // TODO (KR) :
)
object ParsingTable {

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
        expandClosures(expandedGrammar.deDuplicatedNTGroups)

        // TODO (KR) :
        Validated.???
      }

    // =====| Types |=====

    final case class Closure(entries: Set[Closure.Entry])
    object Closure {

      final case class Entry(
          reducesTo: ExpandedGrammar.Identifier.NonTerminal,
          seen: List[ExpandedGrammar.Identifier],
          waiting: List[ExpandedGrammar.Identifier],
      )

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

  }
}
