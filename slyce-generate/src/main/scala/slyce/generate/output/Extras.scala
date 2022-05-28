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
    allTerminals: Set[Extras.Terminal],
    allNTs: Set[Extras.NonTerminal],
)
object Extras {

  final case class With(
      target: ExpandedGrammar.Identifier,
      nt: ExpandedGrammar.Identifier.NonTerminal,
      withType: With.Type,
  )
  object With {
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

  final case class Terminal(
      name: ExpandedGrammar.Identifier.Term,
      withs: Option[NonEmptyList[With]],
  )

  final case class NonTerminal(
      name: ExpandedGrammar.Identifier.NonTerminal,
      withs: Option[NonEmptyList[With]],
      prods: NonTerminal.Productions,
      definedTypes: List[NonTerminal.TypeDefinition],
      ntg: Option[ExpandedGrammar.NTGroup],
  )
  object NonTerminal {

    enum Production {
      case CaseObject
      case CaseClass(elements: NonEmptyList[ExpandedGrammar.Identifier])
    }
    object Production {

      def apply(elements: List[ExpandedGrammar.Identifier]): Production =
        elements.toNel match {
          case Some(elements) => CaseClass(elements)
          case None           => CaseObject
        }

    }

    final case class IndexedProduction(idx: Int, prod: Production)

    enum Productions {
      case Single(prod: Production)
      case Many(prods: NonEmptyList[IndexedProduction])
    }
    object Productions {

      def apply[A](nel: NonEmptyList[A])(f: A => List[ExpandedGrammar.Identifier]): Productions =
        nel.zipWithIndex.map { (a, idx) => IndexedProduction(idx, Production(f(a))) } match {
          case NonEmptyList(IndexedProduction(_, prod), Nil) => Single(prod)
          case prods                                         => Many(prods)
        }

      def apply[A](a0: A, aN: A*)(f: A => List[ExpandedGrammar.Identifier]): Productions =
        Productions(NonEmptyList(a0, aN.toList))(f)

    }

    enum TypeDefinition {
      case Trait(name: With.Type)
      case Type(name: With.Type, equals: ExpandedGrammar.Identifier)
    }

  }

  object build {

    def apply(dfa: DFA, expandedGrammar: ExpandedGrammar): Validated[Extras] = {
      val allProducedTerminals: Set[ExpandedGrammar.Identifier.Term] = calcAllProducedTerminals(dfa)
      val allReferencedTerms: Set[ExpandedGrammar.Identifier.Term] = finalAllReferencedTerms(expandedGrammar)

      // TODO (KR) : Calculate/validate what NFA regular expressions can produce.
      //           : Once this is done, it also might be possible to remove the error type from (String, Span) => Token
      val allTerminals: Set[ExpandedGrammar.Identifier.Term] = allProducedTerminals | allReferencedTerms

      expandedGrammar.deDuplicatedNTGroups.parTraverse(withsFromNTGroup).map { withsList =>
        val allWiths: List[With] = withsList.flatten.distinct
        val withsByNTAndType: Map[(ExpandedGrammar.Identifier.NonTerminal, With.Type), Withs] =
          allWiths.groupByNel(w => (w.nt, w.withType))(Order.by(_.toString)).toMap.map { (k, ws) => (k, Withs(ws)) }
        // TODO (KR) : This needs to filter out all Withs where the id is the only one
        val withsByTarget: Map[ExpandedGrammar.Identifier, NonEmptyList[With]] =
          allWiths.groupByNel(_.target)(Order.by(_.toString))

        Extras(
          allTerminals = allTerminals.map { n => Terminal(n, withsByTarget.get(n)) },
          allNTs = expandedGrammar.deDuplicatedNTGroups.flatMap(convertNTGroup(_, withsByNTAndType, withsByTarget)).toSet,
        )
      }
    }

    private def calcAllProducedTerminals(dfa: DFA): Set[ExpandedGrammar.Identifier.Term] =
      dfa.states
        .flatMap(_.yields)
        .flatMap(_._2.yields.map(_.value))
        .collect { case Yields.Yield.Terminal(name, _, _) => ExpandedGrammar.Identifier.Term.Terminal(name) }
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

    private def typeDefinition(
        nt: ExpandedGrammar.Identifier.NonTerminal,
        name: Extras.With.Type,
        withsByNTAndType: Map[(ExpandedGrammar.Identifier.NonTerminal, With.Type), Withs],
    ): NonTerminal.TypeDefinition =
      withsByNTAndType((nt, name)) match {
        case Withs.One(w)  => NonTerminal.TypeDefinition.Type(name, w.target)
        case Withs.Many(_) => NonTerminal.TypeDefinition.Trait(name)
      }

    private def convertNTGroup(
        ntGroup: ExpandedGrammar.NTGroup,
        withsByNTAndType: Map[(ExpandedGrammar.Identifier.NonTerminal, With.Type), Extras.Withs],
        withsByTarget: Map[ExpandedGrammar.Identifier, NonEmptyList[With]],
    ): List[NonTerminal] = {
      val ntName = ExpandedGrammar.ntGroupHead(ntGroup)
      val withs = withsByTarget.get(ntName)
      ntGroup match {
        case ExpandedGrammar.NTGroup.BasicNT(_, prods) =>
          NonTerminal(
            name = ntName,
            withs = withs,
            prods = NonTerminal.Productions(prods)(identity),
            definedTypes = Nil,
            ntg = ntGroup.some,
          ) :: Nil
        case ExpandedGrammar.NTGroup.LiftNT(_, prods) =>
          NonTerminal(
            name = ntName,
            withs = withs,
            prods = NonTerminal.Productions(prods)(_.toList),
            definedTypes = typeDefinition(ntName, With.Type.Lift, withsByNTAndType) :: Nil,
            ntg = ntGroup.some,
          ) :: Nil
        case ExpandedGrammar.NTGroup.ListNT(name, listType, startProds, repeatProds) =>
          val ntName2 = ExpandedGrammar.listNTId(name, ExpandedGrammar.Identifier.NonTerminal.ListType.Tail)

          val repeat: Option[List[ExpandedGrammar.Identifier]] =
            (listType, repeatProds) match {
              case (GrammarInput.NonTerminal.ListNonTerminal.Type.*, None)              => None
              case (GrammarInput.NonTerminal.ListNonTerminal.Type.*, Some(repeatProds)) => repeatProds.toList.some
              case (GrammarInput.NonTerminal.ListNonTerminal.Type.+, None)              => startProds.toList.some
              case (GrammarInput.NonTerminal.ListNonTerminal.Type.+, Some(repeatProds)) => repeatProds.toList.some
            }

          val prod1 =
            NonTerminal.Productions(
              NonEmptyList(
                startProds.toList :+ (if (repeat.isEmpty) ntName else ntName2),
                listType match {
                  case GrammarInput.NonTerminal.ListNonTerminal.Type.* => Nil :: Nil
                  case GrammarInput.NonTerminal.ListNonTerminal.Type.+ => Nil
                },
              ),
            )(identity)

          NonTerminal(
            name = ntName,
            withs = withs,
            prods = prod1,
            definedTypes = typeDefinition(ntName, With.Type.Lift, withsByNTAndType) :: Nil,
            ntg = ntGroup.some,
          ) ::
            repeat.map { repeat =>
              NonTerminal(
                name = ntName2,
                withs = withsByTarget.get(ntName2),
                prods = NonTerminal.Productions(repeat :+ ntName2, Nil)(identity),
                definedTypes = Nil,
                ntg = None,
              )
            }.toList
        case ExpandedGrammar.NTGroup.AssocNT(name, assocs, base) =>
          def assocNT(idx: Int, op: ExpandedGrammar.Identifier, dir: GrammarInput.NonTerminal.AssocNonTerminal.Type): NonTerminal = {
            val ntName = ExpandedGrammar.assocNTId(name, idx)
            val ntName2 = ExpandedGrammar.assocNTId(name, idx + 1)
            val prod1: List[ExpandedGrammar.Identifier] =
              dir match {
                case GrammarInput.NonTerminal.AssocNonTerminal.Type.Left  => List(ntName, op, ntName2)
                case GrammarInput.NonTerminal.AssocNonTerminal.Type.Right => List(ntName2, op, ntName)
              }
            NonTerminal(
              name = ntName,
              withs = withsByTarget.get(ntName),
              prods = NonTerminal.Productions(prod1, ntName2 :: Nil)(identity),
              definedTypes =
                if (idx == 0) List(typeDefinition(ntName, With.Type.Operand, withsByNTAndType), typeDefinition(ntName, With.Type.Operator, withsByNTAndType))
                else Nil,
              ntg = Option.when(idx == 0)(ntGroup),
            )
          }

          val assocNTs: List[NonTerminal] = assocs.toList.zipWithIndex.map { case ((op, dir), idx) => assocNT(idx + 1, op, dir) }
          val baseName = ExpandedGrammar.assocNTId(name, assocs.size + 1)
          val endNT: NonTerminal =
            NonTerminal(
              name = baseName,
              withs = withsByTarget.get(baseName),
              prods = base match {
                case Left(simple) => NonTerminal.Productions(simple)(identity)
                case Right(lift)  => NonTerminal.Productions(lift)(_.toList)
              },
              definedTypes = Nil,
              ntg = None,
            )

          assocNTs :+ endNT
        case ExpandedGrammar.NTGroup.Optional(id) =>
          NonTerminal(
            name = ntName,
            withs = withs,
            prods = NonTerminal.Productions(id :: Nil, Nil)(identity),
            definedTypes = Nil,
            ntg = ntGroup.some,
          ) :: Nil
      }
    }

  }

}
