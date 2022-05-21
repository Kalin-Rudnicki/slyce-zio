package slyce.generate.grammar

import cats.Functor
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import java.util.UUID
import klib.utils.*
import scala.annotation.tailrec

import slyce.core.*
import slyce.generate.*

final case class ExpandedGrammar private (
    startNt: Marked[String], // TODO (KR) : Remove?
    nts: List[ExpandedGrammar.NT[ExpandedGrammar.Identifier.NonTerminal]],
    aliases: List[ExpandedGrammar.Alias],
    extras: Map[ExpandedGrammar.Identifier.NonTerminal, List[ExpandedGrammar.Extra]],
    withs: List[ExpandedGrammar.With],
)
object ExpandedGrammar {

  final case class NT[+N <: Identifier.NonTerminal](
      name: N,
      productions: NonEmptyList[NT.Production],
  )
  object NT {

    def apply[N <: Identifier.NonTerminal](name: N, reduction0: Production, reductionN: Production*): NT[N] =
      NT(name, NonEmptyList(reduction0, reductionN.toList))

    final case class Production(elements: List[Identifier], liftIdx: Option[Int])
    object Production {
      def apply(elementN: Identifier*): Production = Production(elementN.toList, None)
    }

  }

  sealed trait Identifier
  object Identifier {

    enum NonTerminal extends Identifier {
      case NamedNt(name: String)
      case ListNt(name: String, `type`: NonTerminal.ListType)
      case AnonListNt(key: UUID, `type`: NonTerminal.ListType)
      case AssocNt(name: String, idx: Int)
      case AnonOptNt(identifier: Identifier)
    }
    object NonTerminal {
      enum ListType { case Simple, Head, Tail }
    }

    sealed trait Term extends Identifier
    final case class Terminal(name: String) extends Term
    final case class Raw(name: String) extends Term {
      override def toString: String = s"Raw(${name.unesc})"
    }

  }

  final case class Alias(
      named: Identifier.NonTerminal,
      actual: Identifier.NonTerminal,
  )

  final case class With(
      extendingIdentifier: Identifier,
      typeInNT: Identifier.NonTerminal,
      `type`: With.Type,
  )
  object With {
    enum Type { case Lift, Operator, Operand }
  }

  enum Extra {
    case SimpleToList(liftIdx: Int, tailIdx: Int)
    case HeadTailToList(isNel: Boolean, headLiftIdx: Int, headTailIdx: Int, tailNt: Identifier.NonTerminal, tailLiftIdx: Int, tailTailIdx: Int)
    case Optional
    case Lift(idxs: NonEmptyList[Int])
    case LiftExpr(baseName: String, assocs: NonEmptyList[GrammarInput.NonTerminal.AssocNonTerminal.Type], idxs: NonEmptyList[(Int, Boolean)])
  }

  object fromGrammar {

    def apply(grammar: GrammarInput): Validated[ExpandedGrammar] =
      grammar.nonTerminals
        .parTraverse { nt =>
          expandNonTerminal(nt.name.value, nt.nonTerminal)
        }
        .map { expansions =>
          val combined = Expansion.combine.unit(expansions)
          ExpandedGrammar(
            startNt = grammar.startNT,
            nts = combined.generatedNts,
            aliases = combined.aliases,
            extras = combined.extras.groupMap(_.nt)(_.extra),
            withs = combined.withs.distinct,
          )
        }

    // =====| Types |=====

    private final case class Expansion[+A](
        data: A,
        generatedNts: List[NT[Identifier.NonTerminal]],
        aliases: List[Alias],
        withs: List[With],
        extras: List[ExtraFor],
    ) {

      def map[B](f: A => B): Expansion[B] = Expansion(f(data), generatedNts, aliases, withs, extras)

      def add(
          generatedNts: List[NT[Identifier.NonTerminal]] = Nil,
          aliases: List[Alias] = Nil,
          withs: List[With] = Nil,
          extras: List[ExtraFor] = Nil,
      ): Expansion[A] =
        Expansion(
          data = this.data,
          generatedNts = generatedNts ::: this.generatedNts,
          aliases = aliases ::: this.aliases,
          withs = withs ::: this.withs,
          extras = extras ::: this.extras,
        )

    }
    private object Expansion {

      def join(
          main: Expansion[Identifier],
          extras: Expansion[Identifier]*,
      ): Expansion[Identifier] = {
        val all = main :: extras.toList

        Expansion(
          data = main.data,
          generatedNts = all.flatMap(_.generatedNts),
          aliases = all.flatMap(_.aliases),
          withs = all.flatMap(_.withs),
          extras = all.flatMap(_.extras),
        )
      }

      object combine {

        def apply[L[_]: ToList: Functor, T, T2](expansions: L[Expansion[T]])(f: L[T] => T2): Expansion[T2] = {
          val expansionList = implicitly[ToList[L]].toList(expansions)

          Expansion(
            f(expansions.map(_.data)),
            expansionList.flatMap(_.generatedNts),
            expansionList.flatMap(_.aliases),
            expansionList.flatMap(_.withs),
            expansionList.flatMap(_.extras),
          )
        }

        // TODO (KR) : Remove?
        def unit[L[_]: ToList: Functor, T](expansions: L[Expansion[T]]): Expansion[Unit] = combine(expansions)(_ => ())

      }

    }

    final case class ExtraFor(
        nt: Identifier.NonTerminal,
        extra: Extra,
    )

    final case class ToList[L[_]](toList: [A] => L[A] => List[A])
    object ToList {
      implicit val listToList: ToList[List] = ToList[List] { [A] => (l: List[A]) => l }
      implicit val nonEmptyListToList: ToList[NonEmptyList] = ToList[NonEmptyList] { [A] => (l: NonEmptyList[A]) => l.toList }
    }

    // =====| Helpers |=====

    private def convertGrammarIdentifier(identifier: GrammarInput.Identifier): Identifier =
      identifier match {
        case GrammarInput.Identifier.NonTerminal(name) => Identifier.NonTerminal.NamedNt(name)
        case GrammarInput.Identifier.Terminal(name)    => Identifier.Terminal(name)
        case GrammarInput.Identifier.Raw(text)         => Identifier.Raw(text)
      }

    private def expandNonTerminal(
        name: grammar.GrammarInput.Identifier.NonTerminal,
        nonTerminal: GrammarInput.NonTerminal,
    ): Validated[Expansion[Identifier]] =
      nonTerminal match {
        case snt: GrammarInput.NonTerminal.StandardNonTerminal => expandStandardNonTerminal(Identifier.NonTerminal.NamedNt(name.name), snt, None)
        case lnt: GrammarInput.NonTerminal.ListNonTerminal     => expandListNonTerminal(name.some, lnt)
        case ant: GrammarInput.NonTerminal.AssocNonTerminal    => expandAssocNonTerminal(name, ant)
      }

    private object expandStandardNonTerminal {

      def apply(
          name: Identifier.NonTerminal,
          snt: GrammarInput.NonTerminal.StandardNonTerminal,
          exprExtras: Option[(String, NonEmptyList[GrammarInput.NonTerminal.AssocNonTerminal.Type])],
      ): Validated[Expansion[Identifier]] =
        snt match {
          case nt: GrammarInput.NonTerminal.StandardNonTerminal.`:` => colon(name, nt)
          case nt: GrammarInput.NonTerminal.StandardNonTerminal.^   => carrot(name, nt, exprExtras)
        }

      private def colon(
          name: Identifier.NonTerminal,
          nt: GrammarInput.NonTerminal.StandardNonTerminal.`:`,
      ): Validated[Expansion[Identifier]] = {
        // TODO (KR) : In the previous version, these were both unused vals, returned 'None', and had 'TODO' comments on them.
        //           : The correct course of action is probably to eventually delete these.
        // val mAddWiths: Option[Identifier => With] = ???
        // val lift: Option[ExtraFor] = ???

        nt.productions.parTraverse(expandList).map { eReductions =>
          val tmpENt = Expansion.combine(eReductions)(NT(name, _))
          tmpENt.copy(data = name, generatedNts = tmpENt.data :: tmpENt.generatedNts)
        }
      }

      private def carrot(
          name: Identifier.NonTerminal,
          nt: GrammarInput.NonTerminal.StandardNonTerminal.^,
          exprExtras: Option[(String, NonEmptyList[GrammarInput.NonTerminal.AssocNonTerminal.Type])],
      ): Validated[Expansion[Identifier]] = {
        val mAddWiths: Identifier => Option[With] =
          exprExtras match {
            case Some((n, _)) => {
              case Identifier.NonTerminal.NamedNt(n2) if n == n2 => None
              case id                                            => With(id, Identifier.NonTerminal.AssocNt(n, 1), With.Type.Operand).some
            }
            case None =>
              With(_, name, With.Type.Lift).some
          }
        val lift: ExtraFor =
          exprExtras match {
            case Some((n, assocs)) =>
              val base = Identifier.NonTerminal.AssocNt(n, 1)
              val idxs =
                nt.productions.map { r =>
                  (
                    r.liftIdx,
                    r.lift.value match {
                      case GrammarInput.Identifier.NonTerminal(n2) => n == n2
                      case _                                       => false
                    },
                  )
                }
              ExtraFor(base, Extra.LiftExpr(n, assocs.reverse, idxs))
            case None =>
              ExtraFor(name, Extra.Lift(nt.productions.map(_.liftIdx)))
          }

        nt.productions.parTraverse(expandIgnoredList(_, mAddWiths.some)).map { eReductions =>
          val tmpENt = Expansion.combine(eReductions)(ers => NT(name, ers))
          tmpENt.add(generatedNts = tmpENt.data :: Nil, extras = lift :: Nil).map(_ => name)
        }
      }

    }

    private object expandListNonTerminal {

      def apply(
          name: Option[GrammarInput.Identifier.NonTerminal],
          lnt: GrammarInput.NonTerminal.ListNonTerminal,
      ): Validated[Expansion[Identifier]] =
        (lnt.`type`, lnt.repeat) match {
          case (GrammarInput.NonTerminal.ListNonTerminal.Type.*, None)         => simpleStar(name, lnt.start)
          case (GrammarInput.NonTerminal.ListNonTerminal.Type.*, Some(repeat)) => complexStar(name, lnt.start, repeat)
          case (GrammarInput.NonTerminal.ListNonTerminal.Type.+, None)         => simplePlus(name, lnt.start)
          case (GrammarInput.NonTerminal.ListNonTerminal.Type.+, Some(repeat)) => complexPlus(name, lnt.start, repeat)
        }

      private def createMyId(name: Option[GrammarInput.Identifier.NonTerminal]): (Option[Alias], Identifier.NonTerminal) =
        name match {
          case Some(name) =>
            val id = Identifier.NonTerminal.ListNt(name.name, Identifier.NonTerminal.ListType.Simple)
            (
              Alias(Identifier.NonTerminal.NamedNt(name.name), id).some,
              id,
            )
          case None =>
            (
              None,
              Identifier.NonTerminal.AnonListNt(UUID.randomUUID, Identifier.NonTerminal.ListType.Simple),
            )
        }

      private def createMyIds(name: Option[GrammarInput.Identifier.NonTerminal]): (Option[Alias], Identifier.NonTerminal, Identifier.NonTerminal) =
        name match {
          case Some(name) =>
            val headId = Identifier.NonTerminal.ListNt(name.name, Identifier.NonTerminal.ListType.Head)
            (
              Alias(Identifier.NonTerminal.NamedNt(name.name), headId).some,
              headId,
              Identifier.NonTerminal.ListNt(name.name, Identifier.NonTerminal.ListType.Tail),
            )
          case None =>
            // TODO (KR) : It would be nice to associate the 2 with each other...
            //           : This should be possible by changing all of the de-dupe stuff from using UUID to (UUID, ListType)
            (
              None,
              Identifier.NonTerminal.AnonListNt(UUID.randomUUID, Identifier.NonTerminal.ListType.Head),
              Identifier.NonTerminal.AnonListNt(UUID.randomUUID, Identifier.NonTerminal.ListType.Tail),
            )
        }

      private def simpleStar(
          name: Option[GrammarInput.Identifier.NonTerminal],
          start: LiftList[Marked[GrammarInput.Element]],
      ): Validated[Expansion[Identifier]] = {
        val (ma, myId) = createMyId(name)

        for {
          eStart <- expandIgnoredList(start, Some(With(_, myId, With.Type.Lift).some))
          sR1 = NT.Production(eStart.data.elements.appended(myId), eStart.data.liftIdx)
          sNt = NT(myId, sR1, NT.Production())
        } yield Expansion(
          myId,
          sNt :: eStart.generatedNts,
          ma.toList ::: eStart.aliases,
          eStart.withs,
          ExtraFor(
            nt = myId,
            extra = Extra.SimpleToList(start.liftIdx, start.size),
          ) ::
            eStart.extras,
        )
      }

      private def complexStar(
          name: Option[GrammarInput.Identifier.NonTerminal],
          start: LiftList[Marked[GrammarInput.Element]],
          repeat: LiftList[Marked[GrammarInput.Element]],
      ): Validated[Expansion[Identifier]] = {
        val (ma, myHeadId, myTailId) = createMyIds(name)

        for {
          eStart <- expandIgnoredList(start, Some(With(_, myHeadId, With.Type.Lift).some))
          eRepeat <- expandIgnoredList(repeat, Some(With(_, myHeadId, With.Type.Lift).some))
          sR1 = NT.Production(eStart.data.elements.appended(myTailId), eStart.data.liftIdx)
          rR1 = NT.Production(eRepeat.data.elements.appended(myTailId), eRepeat.data.liftIdx)
          sNt = NT(myHeadId, sR1, NT.Production())
          rNt = NT(myTailId, rR1, NT.Production())
        } yield Expansion(
          myHeadId,
          sNt :: rNt :: eStart.generatedNts ::: eRepeat.generatedNts,
          ma.toList ::: eStart.aliases ::: eRepeat.aliases,
          eStart.withs ::: eRepeat.withs,
          ExtraFor(
            myHeadId,
            Extra.HeadTailToList(
              isNel = false,
              headLiftIdx = start.liftIdx,
              headTailIdx = start.size,
              tailNt = myTailId,
              tailLiftIdx = repeat.liftIdx,
              tailTailIdx = repeat.size,
            ),
          ) :: eStart.extras ::: eRepeat.extras,
        )
      }

      private def simplePlus(
          name: Option[GrammarInput.Identifier.NonTerminal],
          start: LiftList[Marked[GrammarInput.Element]],
      ): Validated[Expansion[Identifier]] = {
        val (ma, myHeadId, myTailId) = createMyIds(name)

        for {
          eStart <- expandIgnoredList(start, Some(With(_, myHeadId, With.Type.Lift).some))
          sR1 = NT.Production(eStart.data.elements.appended(myTailId), eStart.data.liftIdx)
          sNt = NT(myHeadId, sR1)
          rNt = NT(myTailId, sR1, NT.Production())
        } yield Expansion(
          myHeadId,
          sNt :: rNt :: eStart.generatedNts,
          ma.toList ::: eStart.aliases,
          eStart.withs,
          ExtraFor(
            myHeadId,
            Extra.HeadTailToList(
              isNel = true,
              headLiftIdx = start.liftIdx,
              headTailIdx = start.size,
              tailNt = myTailId,
              tailLiftIdx = start.liftIdx,
              tailTailIdx = start.size,
            ),
          ) :: eStart.extras,
        )
      }

      private def complexPlus(
          name: Option[GrammarInput.Identifier.NonTerminal],
          start: LiftList[Marked[GrammarInput.Element]],
          repeat: LiftList[Marked[GrammarInput.Element]],
      ): Validated[Expansion[Identifier]] = {
        val (ma, myHeadId, myTailId) = createMyIds(name)

        for {
          eStart <- expandIgnoredList(start, Some(With(_, myHeadId, With.Type.Lift).some))
          eRepeat <- expandIgnoredList(repeat, Some(With(_, myHeadId, With.Type.Lift).some))
          sR1 = NT.Production(eStart.data.elements.appended(myTailId), eStart.data.liftIdx)
          rR1 = NT.Production(eRepeat.data.elements.appended(myTailId), eRepeat.data.liftIdx)
          sNt = NT(myHeadId, sR1)
          rNt = NT(myTailId, rR1, NT.Production())
        } yield Expansion(
          myHeadId,
          sNt :: rNt :: eStart.generatedNts ::: eRepeat.generatedNts,
          ma.toList ::: eStart.aliases ::: eRepeat.aliases,
          eStart.withs ::: eRepeat.withs,
          ExtraFor(
            myHeadId,
            Extra.HeadTailToList(
              isNel = true,
              headLiftIdx = start.liftIdx,
              headTailIdx = start.size,
              tailNt = myTailId,
              tailLiftIdx = repeat.liftIdx,
              tailTailIdx = repeat.size,
            ),
          ) :: eStart.extras ::: eRepeat.extras,
        )
      }

    }

    private object expandAssocNonTerminal {

      def apply(
          name: GrammarInput.Identifier.NonTerminal,
          ant: GrammarInput.NonTerminal.AssocNonTerminal,
      ): Validated[Expansion[Identifier]] =
        rec(name, ant, 1, ant.assocElements.toList.reverse).map { expansion =>
          expansion.add(
            aliases = Alias(
              Identifier.NonTerminal.NamedNt(name.name),
              Identifier.NonTerminal.AssocNt(name.name, 1),
            ) :: Nil,
          )
        }

      private def rec(
          name: GrammarInput.Identifier.NonTerminal,
          ant: GrammarInput.NonTerminal.AssocNonTerminal,
          idx: Int,
          queue: List[(Marked[GrammarInput.NonTerminal.AssocNonTerminal.Type], Marked[GrammarInput.Element])],
      ): Validated[Expansion[Identifier]] =
        queue match {
          case head :: tail =>
            for {
              childExpansion <- rec(name, ant, idx + 1, tail)
              opExpansion <- expandElement(head._2)
              myId = Identifier.NonTerminal.AssocNt(name.name, idx)
              myExpansion = Expansion(
                myId,
                NT(
                  myId,
                  head._1.value match {
                    case GrammarInput.NonTerminal.AssocNonTerminal.Type.Left =>
                      NT.Production(
                        myId,
                        opExpansion.data,
                        childExpansion.data,
                      )
                    case GrammarInput.NonTerminal.AssocNonTerminal.Type.Right =>
                      NT.Production(
                        childExpansion.data,
                        opExpansion.data,
                        myId,
                      )
                  },
                  NT.Production(
                    childExpansion.data,
                  ),
                ) :: Nil,
                Nil,
                With(opExpansion.data, Identifier.NonTerminal.AssocNt(name.name, 1), With.Type.Operator) :: Nil,
                Nil,
              )
            } yield Expansion.join(
              myExpansion,
              childExpansion,
              opExpansion,
            )
          case Nil =>
            expandStandardNonTerminal(
              Identifier.NonTerminal.AssocNt(name.name, idx),
              ant.base,
              (name.name, ant.assocElements.map(_._1.value)).some,
            )
        }

    }

    private def expandList(l: List[Marked[GrammarInput.Element]]): Validated[Expansion[NT.Production]] =
      l.parTraverse(expandElement(_)).map {
        Expansion.combine(_)(rs => NT.Production(rs*))
      }

    private def expandIgnoredList(
        il: LiftList[Marked[GrammarInput.Element]],
        mWith: Option[Identifier => Option[With]] = None,
    ): Validated[Expansion[NT.Production]] =
      for {
        beforeExpansions <- il.before.parTraverse(expandElement(_))
        unIgnoredExpansion <- expandElement(il.lift, mWith)
        afterExpansions <- il.after.parTraverse(expandElement(_))
        expansions = beforeExpansions ::: unIgnoredExpansion :: afterExpansions
      } yield Expansion.combine(expansions)(rs => NT.Production(rs, il.before.size.some))

    private def expandElement(
        element: Marked[GrammarInput.Element],
        mWith: Option[Identifier => Option[With]] = None,
    ): Validated[Expansion[Identifier]] = {
      def addWithIfExists(expansion: Expansion[Identifier]): Expansion[Identifier] =
        mWith match {
          case Some(withF) => expansion.copy(withs = withF(expansion.data).toList ::: expansion.withs)
          case None        => expansion
        }

      val (isOpt, elem) = element.value.toNonOpt

      expandNonOptElement(elem).map {
        case expandedElement if isOpt =>
          val optId = Identifier.NonTerminal.AnonOptNt(expandedElement.data)
          val optElem = Expansion(
            optId,
            NT(
              optId,
              NT.Production(expandedElement.data),
              NT.Production(),
            ) :: Nil,
            Nil,
            With(expandedElement.data, optId, With.Type.Lift) :: Nil,
            ExtraFor(optId, Extra.Optional) :: Nil,
          )
          Expansion.join(addWithIfExists(optElem), expandedElement)
        case expandedElement =>
          addWithIfExists(expandedElement)
      }
    }

    private def expandNonOptElement(element: GrammarInput.Element.NonOptional): Validated[Expansion[Identifier]] =
      element match {
        case lnt: GrammarInput.NonTerminal.ListNonTerminal => expandListNonTerminal(None, lnt)
        case identifier: GrammarInput.Identifier           => Expansion(convertGrammarIdentifier(identifier), Nil, Nil, Nil, Nil).asRight
      }

  }

  // TODO (KR) : This was jus copied from the previous version.
  //           : It might make sense to give it the clean-up treatment as well.
  def deDuplicate(expandedGrammar: ExpandedGrammar): ExpandedGrammar = {
    val anonListUUIDMap: Map[UUID, NT[Identifier.NonTerminal.AnonListNt]] =
      expandedGrammar.nts.flatMap { nt =>
        nt.name match {
          case anonList: Identifier.NonTerminal.AnonListNt => (anonList.key, nt.asInstanceOf[NT[Identifier.NonTerminal.AnonListNt]]).some
          case _                                           => None
        }
      }.toMap

    def getNonBlockedNts(completedUUIDs: Set[UUID]): List[NT[Identifier.NonTerminal.AnonListNt]] = {
      def validAnonList(nt: NT[Identifier.NonTerminal.AnonListNt]): Boolean = {
        def isAlreadyDone: Boolean =
          completedUUIDs.contains(nt.name.key)

        def isBlocked: Boolean =
          nt.productions.toList.exists { r =>
            r.elements.exists {
              case al: Identifier.NonTerminal.AnonListNt => al.key != nt.name.key && !completedUUIDs.contains(al.key)
              case _                                     => false
            }
          }

        !(isAlreadyDone || isBlocked)
      }

      anonListUUIDMap.values.toList.filter(validAnonList)
    }

    def mDereferenceNtId(key: UUID, id: Identifier.NonTerminal, found: Map[UUID, UUID]): Option[Identifier.NonTerminal] =
      id match {
        case al: Identifier.NonTerminal.AnonListNt =>
          Option.when(al.key != key)(dereferenceNtId(id, found))
        case _ =>
          id.some
      }
    def dereferenceNtId(id: Identifier.NonTerminal, found: Map[UUID, UUID]): Identifier.NonTerminal =
      id match {
        case al: Identifier.NonTerminal.AnonListNt =>
          Identifier.NonTerminal
            .AnonListNt(found.getOrElse(al.key, al.key), al.`type`)
            .asInstanceOf[id.type]
        case _ =>
          id
      }

    def mDereferenceId(key: UUID, id: Identifier, found: Map[UUID, UUID]): Option[Identifier] =
      id match {
        case terminal: Identifier.NonTerminal =>
          mDereferenceNtId(key, terminal, found)
        case _ =>
          id.some
      }
    def dereferenceId(id: Identifier, found: Map[UUID, UUID]): Identifier =
      id match {
        case terminal: Identifier.NonTerminal =>
          dereferenceNtId(terminal, found)
        case _ =>
          id
      }

    def dereferenceNt(
        nt: NT[Identifier.NonTerminal],
        found: Map[UUID, UUID],
    ): NT[Identifier.NonTerminal] =
      NT(
        dereferenceNtId(nt.name, found),
        nt.productions.map(r => NT.Production(r.elements.map(dereferenceId(_, found)), r.liftIdx)),
      )

    @tailrec
    def findDuplicates(
        found: Map[UUID, UUID],
    ): Map[UUID, UUID] = {
      val completedUUIDs = found.keys.toSet
      val nonBlockedNts = getNonBlockedNts(completedUUIDs)

      if (nonBlockedNts.isEmpty) found
      else {
        val nonBlockedDereferenced = nonBlockedNts.map { nt =>
          (
            nt.name.key,
            nt.productions.map(r => (r.elements.map(mDereferenceId(nt.name.key, _, found)), r.liftIdx)),
          )
        }
        val duplicateLists = nonBlockedDereferenced.groupMap(_._2)(_._1).values.toList
        val newMap = duplicateLists.flatMap(dl => dl.map((_, dl.head))).toMap

        findDuplicates(found ++ newMap)
      }
    }

    def filterRedundantAnonListNts(nts: List[NT[Identifier.NonTerminal]], valid: Set[UUID]): List[NT[Identifier.NonTerminal]] =
      nts.filter { nt =>
        nt.name match {
          case al: Identifier.NonTerminal.AnonListNt => valid.contains(al.key)
          case _                                     => true
        }
      }

    val duplicateMap = findDuplicates(Map.empty)
    val filteredNts = filterRedundantAnonListNts(expandedGrammar.nts, duplicateMap.values.toSet).map(dereferenceNt(_, duplicateMap))
    val filteredAliases = expandedGrammar.aliases.map(t => Alias(dereferenceNtId(t.named, duplicateMap), dereferenceNtId(t.actual, duplicateMap)))

    def unaliasNt(nt: ExpandedGrammar.Identifier.NonTerminal): ExpandedGrammar.Identifier.NonTerminal =
      filteredAliases.find(_.named == nt).fold(nt)(_.actual)

    val deReferenceAliases =
      filteredNts.map { nt =>
        ExpandedGrammar.NT(
          nt.name,
          nt.productions.map { reduction =>
            ExpandedGrammar.NT.Production(
              reduction.elements.map {
                case nt: Identifier.NonTerminal => unaliasNt(nt)
                case i                          => i
              },
              reduction.liftIdx,
            )
          },
        )
      }

    val unaliasedWiths =
      expandedGrammar.withs.map { w =>
        With(
          extendingIdentifier = w.extendingIdentifier match {
            case nt: Identifier.NonTerminal => unaliasNt(nt)
            case id                         => id
          },
          typeInNT = w.typeInNT,
          `type` = w.`type`,
        )
      }.distinct

    ExpandedGrammar(
      startNt = expandedGrammar.startNt,
      nts = deReferenceAliases.distinct,
      aliases = filteredAliases,
      extras = expandedGrammar.extras.map { (k, v) => (k, v.distinct) }, // TODO (KR) : unalias as well?
      withs = unaliasedWiths,
    )
  }

}
