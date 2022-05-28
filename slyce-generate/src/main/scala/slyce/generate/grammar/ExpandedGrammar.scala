package slyce.generate.grammar

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import java.util.UUID
import klib.utils.*
import scala.annotation.tailrec

import slyce.core.*
import slyce.generate.*

final case class ExpandedGrammar private (
    startNt: Marked[String],
    maxLookAhead: Marked[Int],
    initialNTGroups: List[ExpandedGrammar.NTGroup],
    deDuplicatedNTGroups: List[ExpandedGrammar.NTGroup],
)
object ExpandedGrammar {

  final case class Production(elements: List[Identifier])
  object Production {
    def apply(elements: Identifier*): Production = Production(elements.toList)
  }

  final case class RawNT(
      name: Identifier.NonTerminal,
      productions: NonEmptyList[Production],
  )
  object RawNT {
    def apply(name: Identifier.NonTerminal, prod0: Production, prodN: Production*): RawNT = RawNT(name, NonEmptyList(prod0, prodN.toList))
  }

  enum NTGroup {
    case BasicNT(
        name: String,
        prods: NonEmptyList[List[Identifier]],
    )
    case LiftNT(
        name: String,
        prods: NonEmptyList[LiftList[Identifier]],
    )
    case ListNT(
        name: Either[String, UUID],
        listType: GrammarInput.NonTerminal.ListNonTerminal.Type,
        startProds: LiftList[Identifier],
        repeatProds: Option[LiftList[Identifier]],
    )
    case AssocNT(
        name: String,
        assocs: NonEmptyList[(Identifier, GrammarInput.NonTerminal.AssocNonTerminal.Type)],
        base: Either[
          NonEmptyList[List[Identifier]],
          NonEmptyList[LiftList[Identifier]],
        ],
    )
    case Optional(
        id: Identifier,
    )

    final lazy val rawNTs: NonEmptyList[RawNT] = convertNTGroup(this)

  }

  sealed trait Identifier
  object Identifier {

    enum NonTerminal extends Identifier {
      case NamedNt(name: String)
      case NamedListNtTail(name: String)
      case AnonListNt(key: UUID, `type`: NonTerminal.ListType)
      case AssocNt(name: String, idx: Int)
      case AnonOptNt(identifier: Identifier)
    }
    object NonTerminal {
      enum ListType { case Simple, Head, Tail }
    }

    sealed trait Term extends Identifier
    object Term {
      final case class Terminal(name: String) extends Term
      final case class Raw(name: String) extends Term {
        override def toString: String = s"Raw(${name.unesc})"
      }
    }

  }

  object fromGrammar {

    def apply(grammar: GrammarInput): ExpandedGrammar = {
      val initialNTGroups: List[NTGroup] = grammar.nonTerminals.flatMap(expandNamedNT)

      ExpandedGrammar(
        startNt = grammar.startNT,
        initialNTGroups = initialNTGroups,
        deDuplicatedNTGroups = removeDuplicates(initialNTGroups),
        maxLookAhead = grammar.maxLookAhead,
      )
    }

    // =====| Types |=====

    private final case class Expansion[+A](
        value: A,
        ntGroups: List[NTGroup],
    ) {
      def map[B](f: A => B): Expansion[B] = Expansion(f(value), ntGroups)
    }
    private object Expansion {

      def mergeNTGroup(ntGroup: NTGroup)(includes: List[Expansion[_]]*): Expansion[Identifier] =
        Expansion(
          ntGroupHead(ntGroup),
          ntGroup :: includes.toList.flatten.flatMap(_.ntGroups),
        )

    }

    final case class AnonListNT(key: UUID, partial: AnonListNT.Partial)
    object AnonListNT {
      final case class Partial(
          listType: GrammarInput.NonTerminal.ListNonTerminal.Type,
          startProds: LiftList[Identifier],
          repeatProds: Option[LiftList[Identifier]],
      )
    }

    // =====| Helpers |=====

    private def convertIdentifier(id: GrammarInput.Identifier): Identifier =
      id match {
        case GrammarInput.Identifier.Terminal(name)    => Identifier.Term.Terminal(name)
        case GrammarInput.Identifier.NonTerminal(name) => Identifier.NonTerminal.NamedNt(name)
        case GrammarInput.Identifier.Raw(text)         => Identifier.Term.Raw(text)
      }

    private def expandNamedNT(namedNT: GrammarInput.NamedNonTerminal): List[NTGroup] = {
      val name: String = namedNT.name.value.name
      namedNT.nonTerminal match {
        case nt: GrammarInput.NonTerminal.StandardNonTerminal => expandStandardNT(name, nt).ntGroups
        case nt: GrammarInput.NonTerminal.ListNonTerminal     => expandListNT(name.some, nt).ntGroups
        case nt: GrammarInput.NonTerminal.AssocNonTerminal    => expandAssocNT(name, nt).ntGroups
      }
    }

    private def expandStandardNT(name: String, standardNT: GrammarInput.NonTerminal.StandardNonTerminal): Expansion[Identifier] =
      standardNT match {
        case GrammarInput.NonTerminal.StandardNonTerminal.`:`(productions) =>
          val expanded: NonEmptyList[Expansion[List[Identifier]]] = productions.map(r => expandList(r.map(_.value)))
          Expansion.mergeNTGroup(
            NTGroup.BasicNT(
              name = name,
              prods = expanded.map(_.value),
            ),
          )(
            expanded.toList,
          )
        case GrammarInput.NonTerminal.StandardNonTerminal.^(productions) =>
          val expanded: NonEmptyList[Expansion[LiftList[Identifier]]] = productions.map(r => expandLiftList(r.map(_.value)))
          Expansion.mergeNTGroup(
            NTGroup.LiftNT(
              name = name,
              prods = expanded.map(_.value),
            ),
          )(
            expanded.toList,
          )
      }

    private def expandListNT(name: Option[String], listNT: GrammarInput.NonTerminal.ListNonTerminal): Expansion[Identifier] = {
      val expandedStart: Expansion[LiftList[Identifier]] = expandLiftList(listNT.start.map(_.value))
      val expandedRepeat: Option[Expansion[LiftList[Identifier]]] = listNT.repeat.map(repeat => expandLiftList(repeat.map(_.value)))

      Expansion.mergeNTGroup(
        NTGroup.ListNT(
          name = name.toLeft(UUID.randomUUID),
          listType = listNT.`type`,
          startProds = expandedStart.value,
          repeatProds = expandedRepeat.map(_.value),
        ),
      )(
        expandedStart :: Nil,
        expandedRepeat.toList,
      )
    }

    private def expandAssocNT(name: String, assocNT: GrammarInput.NonTerminal.AssocNonTerminal): Expansion[Identifier] = {
      val expandedAssocs: NonEmptyList[Expansion[(Identifier, GrammarInput.NonTerminal.AssocNonTerminal.Type)]] =
        assocNT.assocElements.map { (t, e) =>
          expandElement(e.value).map((_, t.value))
        }
      val expandedBase: Expansion[Either[NonEmptyList[List[Identifier]], NonEmptyList[LiftList[Identifier]]]] =
        assocNT.base match {
          case GrammarInput.NonTerminal.StandardNonTerminal.`:`(productions) =>
            val expanded: NonEmptyList[Expansion[List[Identifier]]] = productions.map(r => expandList(r.map(_.value)))
            Expansion(
              expanded.map(_.value).asLeft,
              expanded.toList.flatMap(_.ntGroups),
            )
          case GrammarInput.NonTerminal.StandardNonTerminal.^(productions) =>
            val expanded: NonEmptyList[Expansion[LiftList[Identifier]]] = productions.map(r => expandLiftList(r.map(_.value)))
            Expansion(
              expanded.map(_.value).asRight,
              expanded.toList.flatMap(_.ntGroups),
            )
        }

      Expansion.mergeNTGroup(
        NTGroup.AssocNT(
          name = name,
          assocs = expandedAssocs.map(_.value),
          base = expandedBase.value,
        ),
      )(
        expandedAssocs.toList,
        expandedBase :: Nil,
      )
    }

    private def expandList(list: List[GrammarInput.Element]): Expansion[List[Identifier]] = {
      val expanded: List[Expansion[Identifier]] = list.map(expandElement)
      Expansion(
        expanded.map(_.value),
        expanded.flatMap(_.ntGroups),
      )
    }

    private def expandLiftList(list: LiftList[GrammarInput.Element]): Expansion[LiftList[Identifier]] = {
      val expanded: LiftList[Expansion[Identifier]] = list.map(expandElement)
      Expansion(
        expanded.map(_.value),
        expanded.toList.flatMap(_.ntGroups),
      )
    }

    private def expandElement(element: GrammarInput.Element): Expansion[Identifier] =
      element match {
        case element: GrammarInput.Element.NonOptional =>
          expandNonOptElement(element)
        case GrammarInput.Element.Optional(child) =>
          val expanded: Expansion[Identifier] = expandNonOptElement(child)
          Expansion.mergeNTGroup(
            NTGroup.Optional(expanded.value),
          )(
            expanded :: Nil,
          )
      }

    private def expandNonOptElement(element: GrammarInput.Element.NonOptional): Expansion[Identifier] =
      element match {
        case identifier: GrammarInput.Identifier              => Expansion(convertIdentifier(identifier), Nil)
        case listNT: GrammarInput.NonTerminal.ListNonTerminal => expandListNT(None, listNT)
      }

    private def replaceIdentifier(identifier: Identifier, map: Map[UUID, UUID]): Identifier =
      identifier match {
        case Identifier.NonTerminal.AnonListNt(key, listType) => Identifier.NonTerminal.AnonListNt(map.getOrElse(key, key), listType)
        case id                                               => id
      }

    private def removeDuplicates(ntGroups: List[NTGroup]): List[NTGroup] = {
      val (anonListNTs, otherNTs) = ntGroups.partitionMap {
        case NTGroup.ListNT(Right(key), listType, startProds, repeatProds) => AnonListNT(key, AnonListNT.Partial(listType, startProds, repeatProds)).asLeft
        case other                                                         => other.asRight
      }

      val (newAnonListNts, map) = deDuplicateAnonListNTs(anonListNTs, Map.empty)

      newAnonListNts ::: otherNTs.map(replaceNTGroup(_, map)).distinct
    }

    // NOTE : I think that doing 'map ++ newMappings' is fine,
    //      : but if there are issues, it might be necessary merge them in a more complex way.
    @tailrec
    private def deDuplicateAnonListNTs(
        anonListNTs: List[AnonListNT],
        map: Map[UUID, UUID],
    ): (List[NTGroup.ListNT], Map[UUID, UUID]) = {
      val grouped: List[(AnonListNT.Partial, NonEmptyList[UUID])] =
        anonListNTs.groupMap(_.partial)(_.key).toList.map { (partial, uuids) => (partial, NonEmptyList(uuids.head, uuids.tail)) }

      val newMappings: Map[UUID, UUID] =
        grouped.flatMap { (_, uuids) =>
          uuids.tail.map((_, uuids.head))
        }.toMap

      if (newMappings.isEmpty)
        (
          anonListNTs.map { nt => NTGroup.ListNT(nt.key.asRight, nt.partial.listType, nt.partial.startProds, nt.partial.repeatProds) },
          map,
        )
      else {
        val remaining: List[AnonListNT] =
          grouped.map { (partial, uuids) => AnonListNT(uuids.head, partial) }
        val replaced: List[AnonListNT] =
          remaining.map { case AnonListNT(key, AnonListNT.Partial(listType, startProds, repeatProds)) =>
            AnonListNT(
              key,
              AnonListNT.Partial(
                listType,
                startProds.map(replaceIdentifier(_, newMappings)),
                repeatProds.map(_.map(replaceIdentifier(_, newMappings))),
              ),
            )
          }

        deDuplicateAnonListNTs(
          replaced,
          map ++ newMappings,
        )
      }
    }

    private def replaceNTGroup(ntGroup: NTGroup, map: Map[UUID, UUID]): NTGroup =
      ntGroup match {
        case NTGroup.BasicNT(name, prods) =>
          NTGroup.BasicNT(
            name,
            prods.map(_.map(replaceIdentifier(_, map))),
          )
        case NTGroup.LiftNT(name, prods) =>
          NTGroup.LiftNT(
            name,
            prods.map(_.map(replaceIdentifier(_, map))),
          )
        case NTGroup.ListNT(name, listType, startProds, repeatProds) =>
          NTGroup.ListNT(
            name,
            listType,
            startProds.map(replaceIdentifier(_, map)),
            repeatProds.map(_.map(replaceIdentifier(_, map))),
          )
        case NTGroup.AssocNT(name, assocs, base) =>
          NTGroup.AssocNT(
            name,
            assocs.map { (id, t) => (replaceIdentifier(id, map), t) },
            base match {
              case Right(value) => value.map(_.map(replaceIdentifier(_, map))).asRight
              case Left(value)  => value.map(_.map(replaceIdentifier(_, map))).asLeft
            },
          )
        case NTGroup.Optional(id) =>
          NTGroup.Optional(replaceIdentifier(id, map))
      }

  }

  private def listNTId(name: Either[String, UUID], listType: Identifier.NonTerminal.ListType): Identifier.NonTerminal =
    name match {
      case Left(name) =>
        listType match {
          case Identifier.NonTerminal.ListType.Simple => Identifier.NonTerminal.NamedNt(name)
          case Identifier.NonTerminal.ListType.Head   => Identifier.NonTerminal.NamedNt(name)
          case Identifier.NonTerminal.ListType.Tail   => Identifier.NonTerminal.NamedListNtTail(name)
        }
      case Right(key) => Identifier.NonTerminal.AnonListNt(key, listType)
    }

  private def assocNTId(name: String, idx: Int): Identifier.NonTerminal =
    if (idx == 1) Identifier.NonTerminal.NamedNt(name)
    else Identifier.NonTerminal.AssocNt(name, idx)

  private object convertNTGroup {

    def apply(ntGroup: NTGroup): NonEmptyList[RawNT] =
      ntGroup match {
        case NTGroup.BasicNT(name, prods) => NonEmptyList.one(RawNT(Identifier.NonTerminal.NamedNt(name), prods.map(Production(_))))
        case NTGroup.LiftNT(name, prods)  => NonEmptyList.one(RawNT(Identifier.NonTerminal.NamedNt(name), prods.map(p => Production(p.toList))))
        case listNT: NTGroup.ListNT       => convertListNT(listNT)
        case assocNT: NTGroup.AssocNT     => convertAssocNT(assocNT)
        case NTGroup.Optional(id)         => NonEmptyList.one(RawNT(Identifier.NonTerminal.AnonOptNt(id), Production(id), Production()))
      }

    private def convertListNT(listNT: NTGroup.ListNT): NonEmptyList[RawNT] =
      (listNT.listType, listNT.repeatProds) match {
        case (GrammarInput.NonTerminal.ListNonTerminal.Type.*, None) =>
          val n = listNTId(listNT.name, Identifier.NonTerminal.ListType.Simple)
          NonEmptyList.of(
            RawNT(
              n,
              Production(listNT.startProds.toList :+ n),
              Production(),
            ),
          )
        case (GrammarInput.NonTerminal.ListNonTerminal.Type.+, None) =>
          val n1 = listNTId(listNT.name, Identifier.NonTerminal.ListType.Head)
          val n2 = listNTId(listNT.name, Identifier.NonTerminal.ListType.Tail)
          val p1 = Production(listNT.startProds.toList :+ n2)
          NonEmptyList.of(
            RawNT(
              n1,
              p1,
            ),
            RawNT(
              n2,
              p1,
              Production(),
            ),
          )
        case (GrammarInput.NonTerminal.ListNonTerminal.Type.*, Some(repeatProds)) =>
          val n1 = listNTId(listNT.name, Identifier.NonTerminal.ListType.Head)
          val n2 = listNTId(listNT.name, Identifier.NonTerminal.ListType.Tail)
          NonEmptyList.of(
            RawNT(
              n1,
              Production(listNT.startProds.toList :+ n2),
              Production(),
            ),
            RawNT(
              n2,
              Production(repeatProds.toList :+ n2),
              Production(),
            ),
          )
        case (GrammarInput.NonTerminal.ListNonTerminal.Type.+, Some(repeatProds)) =>
          val n1 = listNTId(listNT.name, Identifier.NonTerminal.ListType.Head)
          val n2 = listNTId(listNT.name, Identifier.NonTerminal.ListType.Tail)
          NonEmptyList.of(
            RawNT(
              n1,
              Production(listNT.startProds.toList :+ n2),
            ),
            RawNT(
              n2,
              Production(repeatProds.toList :+ n2),
              Production(),
            ),
          )
      }

    private def convertAssocNT(assocNT: NTGroup.AssocNT): NonEmptyList[RawNT] = {
      val assocNTs: NonEmptyList[RawNT] =
        assocNT.assocs.zipWithIndex.map { case ((op, assocType), idx) =>
          val myId = assocNTId(assocNT.name, idx + 1)
          val nextId = assocNTId(assocNT.name, idx + 2)
          RawNT(
            myId,
            assocType match {
              case GrammarInput.NonTerminal.AssocNonTerminal.Type.Left  => Production(myId, op, nextId)
              case GrammarInput.NonTerminal.AssocNonTerminal.Type.Right => Production(nextId, op, myId)
            },
            Production(nextId),
          )
        }
      val baseNT: RawNT =
        RawNT(
          Identifier.NonTerminal.AssocNt(assocNT.name, assocNTs.size + 1),
          assocNT.base match {
            case Left(basic) => basic.map(Production(_))
            case Right(lift) => lift.map(p => Production(p.toList))
          },
        )

      assocNTs :+ baseNT
    }

  }

  private[generate] def ntGroupHead(ntGroup: NTGroup): Identifier.NonTerminal =
    ntGroup match {
      case NTGroup.BasicNT(name, _) => Identifier.NonTerminal.NamedNt(name)
      case NTGroup.LiftNT(name, _)  => Identifier.NonTerminal.NamedNt(name)
      case NTGroup.ListNT(name, listType, _, repeatProds) =>
        listNTId(
          name,
          (listType, repeatProds) match {
            case (GrammarInput.NonTerminal.ListNonTerminal.Type.*, None)    => Identifier.NonTerminal.ListType.Simple
            case (GrammarInput.NonTerminal.ListNonTerminal.Type.+, None)    => Identifier.NonTerminal.ListType.Head
            case (GrammarInput.NonTerminal.ListNonTerminal.Type.*, Some(_)) => Identifier.NonTerminal.ListType.Head
            case (GrammarInput.NonTerminal.ListNonTerminal.Type.+, Some(_)) => Identifier.NonTerminal.ListType.Head
          },
        )
      case NTGroup.AssocNT(name, _, _) => assocNTId(name, 1)
      case NTGroup.Optional(id)        => Identifier.NonTerminal.AnonOptNt(id)
    }

}
