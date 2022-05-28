package slyce.generate.output

import cats.Order
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.list.*
import cats.syntax.option.*
import cats.syntax.parallel.*

import slyce.core.*
import slyce.generate.grammar.*
import slyce.generate.lexer.*

final case class Extras private (
    allTerminals: Set[String],
    allRawTerminals: Set[String],
    withsByTarget: Map[ExpandedGrammar.Identifier, NonEmptyList[Extras.With]],
    withsByNTAndType: Map[(ExpandedGrammar.Identifier.NonTerminal, Extras.With.Type), Extras.Withs],
)
object Extras {

  final case class With(
      target: ExpandedGrammar.Identifier,
      nt: ExpandedGrammar.Identifier.NonTerminal,
      withType: With.Type,
  )
  object With {

    enum Target {
      case Terminal(id: ExpandedGrammar.Identifier.Term)
      case NonTerminal(id: ExpandedGrammar.Identifier.NonTerminal)
    }

    enum Type { case Lift, Operand, Operator }

  }

  enum Withs {
    case One(w: With)
    case Many(ws: NonEmptyList[With])
  }
  object Withs {

    def apply(withs: NonEmptyList[With]): Withs =
      withs match {
        case NonEmptyList(w, Nil) => Withs.One(w)
        case withs                => Withs.Many(withs)
      }

  }

  object build {

    def apply(dfa: DFA, expandedGrammar: ExpandedGrammar): Validated[Extras] = {
      val allProducedTerminals: Set[String] = calcAllProducedTerminals(dfa)
      val allReferencedTerms: Set[ExpandedGrammar.Identifier.Term] = finalAllReferencedTerms(expandedGrammar)
      val (allReferencedTerminals: Set[String], allReferencedRawTerminals: Set[String]) =
        allReferencedTerms.partitionMap {
          case ExpandedGrammar.Identifier.Term.Terminal(name) => name.asRight
          case ExpandedGrammar.Identifier.Term.Raw(name)      => name.asLeft
        }

      val allTerminals: Set[String] = allProducedTerminals | allReferencedTerminals
      // TODO (KR) : Calculate/validate what NFA regular expressions can produce.
      //           : Once this is done, it also might be possible to remove the error type from (String, Span) => Token
      val allRawTerminals: Set[String] = allReferencedRawTerminals

      expandedGrammar.deDuplicatedNTGroups.parTraverse(withsFromNTGroup).map { withsList =>
        val allWiths: List[With] = withsList.flatten.distinct

        Extras(
          allTerminals = allTerminals,
          allRawTerminals = allRawTerminals,
          withsByTarget = allWiths.groupByNel(_.target)(Order.allEqual),
          withsByNTAndType = allWiths.groupByNel(w => (w.nt, w.withType))(Order.allEqual).toMap.map { (k, ws) => (k, Withs(ws)) },
        )
      }
    }

    private def calcAllProducedTerminals(dfa: DFA): Set[String] =
      dfa.states
        .flatMap(_.yields)
        .flatMap(_._2.yields.map(_.value))
        .collect { case Yields.Yield.Terminal(name, _, _) => name }
        .toSet

    private def finalAllReferencedTerms(expandedGrammar: ExpandedGrammar): Set[ExpandedGrammar.Identifier.Term] =
      expandedGrammar.deDuplicatedNTGroups
        .flatMap(_.rawNTs.toList)
        .flatMap(_.productions.toList)
        .flatMap(_.elements)
        .collect { case term: ExpandedGrammar.Identifier.Term => term }
        .toSet

    private def withsFromNTGroup(ntGroup: ExpandedGrammar.NTGroup): Validated[List[With]] = {
      val ntGroupHead: ExpandedGrammar.Identifier.NonTerminal = ExpandedGrammar.ntGroupHead(ntGroup)
      ntGroup match {
        case ExpandedGrammar.NTGroup.BasicNT(_, _) => Nil.asRight
        case ExpandedGrammar.NTGroup.LiftNT(_, prods) =>
          prods.toList.map { prod => With(prod.lift, ntGroupHead, With.Type.Lift) }.asRight
        case ExpandedGrammar.NTGroup.ListNT(_, _, startProds, repeatProds) =>
          (startProds :: repeatProds.toList).map { prod => With(prod.lift, ntGroupHead, With.Type.Lift) }.asRight
        case ExpandedGrammar.NTGroup.AssocNT(name, assocs, base) =>
          val withOperators: List[With] =
            assocs.toList.map { op => With(op._1, ntGroupHead, With.Type.Operator) }
          val withOperands: Validated[List[With]] =
            base match {
              case Left(_) => List(With(ExpandedGrammar.Identifier.NonTerminal.AssocNt(name, assocs.size + 1), ntGroupHead, With.Type.Operand)).asRight
              case Right(liftProds) =>
                val all = liftProds.toList.map { prod => With(prod.lift, ntGroupHead, With.Type.Operand) }
                val filtered = all.filter(_.target != ntGroupHead)
                if (filtered.nonEmpty) filtered.asRight
                else Marked(s"AssocNT $name only lifts to itself", Span.Unknown).leftNel
            }

          withOperands.map { _ ::: withOperators }
        case ExpandedGrammar.NTGroup.Optional(_) => Nil.asRight
      }
    }

  }

}
