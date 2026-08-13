package slyce.generate.grammar

import oxygen.predef.core.*
import scala.annotation.tailrec

import slyce.generate.Helpers

/** LR parse table with nested look-ahead (ported/cleaned from v1 ParsingTable). Symbols are GSym; no stringly grammar DSL.
  */
private[slyce] final case class ParsingTable(
    states: List[ParsingTable.ParseState],
)
private[slyce] object ParsingTable {

  final case class ParseState(
      id: Int,
      goto: Map[GSym.NonTerm, Int],
      lookAhead: Action,
  )

  enum Action {
    case Accept
    case Reduce(nt: GSym.NonTerm, prodIdx: Int)
    case Shift(toState: Int)
    case LookAhead(
        onTerm: Map[GSym.Term, Action],
        onEOF: Option[Action], // Accept | Reduce only
    )
  }

  def fromExpandedGrammar(grammar: ExpandedGrammar): Either[String, ParsingTable] = {
    if grammar.maxLookAhead < 1 then return Left("maxLookAhead must be >= 1")

    val raw = grammar.rawNTs
    val defined: Map[GSym.NonTerm, List[Closure.Production]] =
      raw.map { nt =>
        val prods = nt.productions.toList.zipWithIndex.map { case (p, idx) =>
          Closure.Production(ReducesTo.Production(nt.name, idx), p.elements)
        }
        nt.name -> prods
      }.toMap

    val referenced: Set[GSym.NonTerm] =
      raw.flatMap(_.productions.toList.flatMap(_.elements)).collect { case nt: GSym.NonTerm => nt }.toSet

    val missing = referenced -- defined.keySet
    if missing.nonEmpty then return Left(s"Undefined NTs: ${missing.map(_.label).mkString(", ")}")
    if !defined.contains(grammar.startNt) then return Left(s"Start NT not defined: ${grammar.startNt.label}")

    val initialEntry =
      Closure.Entry(
        reducesTo = ReducesTo.Start,
        seen = Nil,
        waiting = grammar.startNt :: Nil,
        lookAhead = Follow(Set.empty, eofIsValid = true) :: Nil,
      )
    val initialClosure = expandEntries(defined, Set(initialEntry), grammar.maxLookAhead)

    val allClosures: List[Closure] =
      Helpers
        .findAll(Set(initialClosure)) { c =>
          calcTransitionMap(defined, c, grammar.maxLookAhead).values.toSet
        }
        .toList

    val pairs: Either[String, List[(Closure, TmpState)]] =
      allClosures.foldLeft[Either[String, List[(Closure, TmpState)]]](Right(Nil)) { (acc, c) =>
        acc.flatMap { list =>
          calcActionState(defined, c, grammar.maxLookAhead).map(s => (c, s) :: list)
        }
      }

    pairs.map { ps =>
      val map = ps.toMap
      val initial = map(initialClosure)
      val others = map.values.toSet - initial
      val stateList = (initial :: others.toList).zipWithIndex
      val actionToId = stateList.toMap
      val closureToId = map.map { case (c, as) => c -> actionToId(as) }

      val states = stateList.map { case (as, id) =>
        ParseState(
          id = id,
          goto = as.goto.map { case (nt, toC) => nt -> closureToId(toC) },
          lookAhead = convertLookAhead(closureToId, as.lookAhead),
        )
      }
      ParsingTable(states)
    }
  }

  // =====| Internal types |=====

  private enum ReducesTo {
    case Start
    case Production(nt: GSym.NonTerm, idx: Int)
  }

  private final case class Follow(validTerminals: Set[GSym.Term], eofIsValid: Boolean)

  private final case class Closure(entries: Set[Closure.Entry]) {
    lazy val finished: Set[Closure.Entry.Finished] = entries.collect { case e: Closure.Entry.Finished => e }
    lazy val unfinished: Set[Closure.Entry.Waiting] = entries.collect { case e: Closure.Entry.Waiting => e }
  }
  private object Closure {
    final case class Production(reducesTo: ReducesTo.Production, produces: List[GSym])

    sealed trait Entry {
      def reducesTo: ReducesTo
      def seen: List[GSym]
      def lookAhead: List[Follow]
      def waitingList: List[GSym]
    }
    object Entry {
      final case class Finished(reducesTo: ReducesTo, seen: List[GSym], lookAhead: List[Follow]) extends Entry {
        override def waitingList: List[GSym] = Nil
      }
      final case class Waiting(reducesTo: ReducesTo, seen: List[GSym], waiting: NonEmptyList[GSym], lookAhead: List[Follow]) extends Entry {
        override def waitingList: List[GSym] = waiting.toList
      }

      def apply(reducesTo: ReducesTo, seen: List[GSym], waiting: List[GSym], lookAhead: List[Follow]): Entry =
        NonEmptyList.fromList(waiting) match {
          case Some(w) => Waiting(reducesTo, seen, w, lookAhead)
          case None    => Finished(reducesTo, seen, lookAhead)
        }
    }
  }

  private final case class TmpState(
      goto: Map[GSym.NonTerm, Closure],
      lookAhead: TmpAction,
  )

  private enum TmpAction {
    case Accept
    case Reduce(prod: Closure.Production)
    case Shift(to: Closure)
    case LookAhead(onTerm: Map[GSym.Term, TmpAction], onEOF: Option[TmpAction])
  }

  // =====| Algorithm (v1-style, cleaned) |=====

  private def mergeFollows(follows: List[List[Follow]]): List[Follow] = {
    val nonEmpty = follows.flatMap(NonEmptyList.fromList)
    NonEmptyList.fromList(nonEmpty) match {
      case Some(nels) =>
        val heads = nels.map(_.head)
        val tails = nels.toList.map(_.tail)
        Follow(heads.toList.toSet.flatMap(_.validTerminals), heads.exists(_.eofIsValid)) :: mergeFollows(tails)
      case None => Nil
    }
  }

  private def calcLookAhead(
      productionsForNT: Map[GSym.NonTerm, List[Closure.Production]],
      ids: List[(ReducesTo, Int, GSym)],
      alreadyExpanded: Set[(ReducesTo, Int)],
      ifPassThrough: List[Follow],
      maxLookAhead: Int,
  ): List[Follow] =
    if maxLookAhead <= 0 then Nil
    else
      ids match {
        case Nil                  => ifPassThrough.take(maxLookAhead)
        case (rt, sc, id) :: tail =>
          id match {
            case nt: GSym.NonTerm =>
              if alreadyExpanded.contains((rt, sc)) then Nil
              else {
                val newExpanded = alreadyExpanded + (rt -> sc)
                val inlined: List[List[(ReducesTo, Int, GSym)]] =
                  productionsForNT.getOrElse(nt, Nil).map { case Closure.Production(prt, waiting) =>
                    waiting.zipWithIndex.map { case (sym, idx) => (prt: ReducesTo, idx, sym) }
                  }
                mergeFollows(inlined.map(more => calcLookAhead(productionsForNT, more ::: tail, newExpanded, ifPassThrough, maxLookAhead)))
              }
            case t: GSym.Term =>
              Follow(Set(t), eofIsValid = false) :: calcLookAhead(productionsForNT, tail, Set.empty, ifPassThrough, maxLookAhead - 1)
          }
      }

  private def expandEntries(
      productionsForNT: Map[GSym.NonTerm, List[Closure.Production]],
      initial: Set[Closure.Entry],
      maxLookAhead: Int,
  ): Closure = {
    val preJoined =
      Helpers.findAll(initial) {
        case Closure.Entry.Waiting(rt, seen, NonEmptyList(next: GSym.NonTerm, waiting), lookAhead) =>
          val lookup = productionsForNT.getOrElse(next, Nil)
          val newIds = waiting.zipWithIndex.map { case (id, idx) => (rt, seen.size + 1 + idx, id) }
          val newFollows = calcLookAhead(productionsForNT, newIds, Set.empty, lookAhead, maxLookAhead)
          lookup.map { case Closure.Production(prod, waitingElems) =>
            Closure.Entry(prod, Nil, waitingElems, newFollows)
          }.toSet
        case _ => Set.empty
      }

    val joined =
      preJoined
        .groupMap(e => (e.reducesTo, e.seen, e.waitingList))(_.lookAhead)
        .toSet
        .map { case ((rt, seen, waiting), follows) =>
          Closure.Entry(rt, seen, waiting, mergeFollows(follows.toList))
        }

    Closure(joined)
  }

  private def calcTransitionMap(
      productionsForNT: Map[GSym.NonTerm, List[Closure.Production]],
      closure: Closure,
      maxLookAhead: Int,
  ): Map[GSym, Closure] =
    closure.unfinished
      .map { case Closure.Entry.Waiting(rt, seen, NonEmptyList(next, stillWaiting), lookAhead) =>
        (next, Closure.Entry(rt, seen :+ next, stillWaiting, lookAhead))
      }
      .groupMap(_._1)(_._2)
      .map { case (id, entries) => id -> expandEntries(productionsForNT, entries, maxLookAhead) }

  private def calcFollowsForClosure(
      productionsForNT: Map[GSym.NonTerm, List[Closure.Production]],
      c: Closure,
      maxLookAhead: Int,
  ): List[Follow] =
    mergeFollows(
      c.entries.toList.map { e =>
        calcLookAhead(
          productionsForNT,
          e.waitingList.zipWithIndex.map { case (id, idx) => (e.reducesTo, e.seen.size + idx, id) },
          Set.empty,
          e.lookAhead,
          maxLookAhead,
        )
      },
    )

  private def calcActionState(
      productionsForNT: Map[GSym.NonTerm, List[Closure.Production]],
      closure: Closure,
      maxLookAhead: Int,
  ): Either[String, TmpState] = {
    val transitions = calcTransitionMap(productionsForNT, closure, maxLookAhead)
    val (ntList, tList) =
      transitions.partitionMap {
        case (nt: GSym.NonTerm, c) => Left(nt -> c)
        case (t: GSym.Term, c)     => Right(t -> (c, calcFollowsForClosure(productionsForNT, c, maxLookAhead)))
      }
    val termMap = tList.toMap
    calcTerminalActions(termMap, closure.finished, Nil).map { la =>
      TmpState(ntList.toMap, la)
    }
  }

  private def calcTerminalActions(
      terminalTransitionMap: Map[GSym.Term, (Closure, List[Follow])],
      finishedEntries: Set[Closure.Entry.Finished],
      rFollowedPath: List[GSym.Term],
  ): Either[String, TmpAction] = {
    val (fesWithout, fesWith) =
      finishedEntries.partitionMap { e =>
        NonEmptyList.fromList(e.lookAhead) match {
          case Some(nel) =>
            Right(nel.head -> Closure.Entry.Finished(e.reducesTo, e.seen, nel.tail))
          case None => Left(e)
        }
      }

    if fesWithout.nonEmpty then {
      val path = rFollowedPath.reverse.map(_.label).mkString(", ")
      val conflicts = fesWithout.map(fe => s"${fe.reducesTo}/${fe.seen.size}").mkString(", ")
      Left(s"Need more look-ahead (path: $path; conflicts: $conflicts). Try increasing maxLookAhead.")
    } else if fesWith.isEmpty then {
      val onTerm = terminalTransitionMap.map { case (t, (c, _)) => t -> TmpAction.Shift(c) }
      Right(TmpAction.LookAhead(onTerm, None))
    } else {
      val fesByTerm: Map[GSym.Term, Set[Closure.Entry.Finished]] =
        fesWith
          .flatMap { case (follow, finished) => follow.validTerminals.toList.map(_ -> finished) }
          .groupMap(_._1)(_._2)

      val fesEOF: Set[Closure.Entry.Finished] =
        fesWith.collect { case (Follow(_, true), finish) => finish }

      val onEOF: Either[String, Option[TmpAction]] =
        fesEOF.toList match {
          case Nil          => Right(None)
          case value :: Nil =>
            value.reducesTo match {
              case ReducesTo.Start          => Right(Some(TmpAction.Accept))
              case rt: ReducesTo.Production => Right(Some(TmpAction.Reduce(Closure.Production(rt, value.seen))))
            }
          case values => Left(s"Multiple EOF actions: $values")
        }

      val onTermPart: Either[String, Map[GSym.Term, TmpAction]] =
        fesByTerm.toList
          .foldLeft[Either[String, List[(GSym.Term, TmpAction)]]](Right(Nil)) { case (acc, (t, fes)) =>
            acc.flatMap { list =>
              (fes.toList, terminalTransitionMap.get(t)) match {
                case (Nil, Some((c, _))) =>
                  Right((t -> TmpAction.Shift(c)) :: list)
                case (fe :: Nil, None) =>
                  fe.reducesTo match {
                    case prod: ReducesTo.Production =>
                      Right((t -> TmpAction.Reduce(Closure.Production(prod, fe.seen))) :: list)
                    case ReducesTo.Start =>
                      Left("Unexpected reduce-to-start on terminal")
                  }
                case (fes2, None) =>
                  calcTerminalActions(Map.empty, fes2.toSet, t :: rFollowedPath).map(a => (t -> a) :: list)
                case (fes2, Some((c, cfs))) =>
                  NonEmptyList.fromList(cfs) match {
                    case Some(nel) =>
                      val filtered = nel.head.validTerminals.toList.map(tt => tt -> (c, nel.tail)).toMap
                      calcTerminalActions(filtered, fes2.toSet, t :: rFollowedPath).map(a => (t -> a) :: list)
                    case None =>
                      Left(s"No more look-ahead for closure on ${t.label}")
                  }
                case _ => Left(s"Unhandled action conflict on ${t.label}")
              }
            }
          }
          .map(_.toMap)

      for {
        partial1 <- onTermPart
        eof <- onEOF
      } yield {
        val referenced = fesWith.flatMap(_._1.validTerminals)
        val partial2 =
          terminalTransitionMap.iterator
            .filterNot { case (t, _) => referenced.contains(t) }
            .map { case (t, (c, _)) => t -> TmpAction.Shift(c) }
            .toMap
        TmpAction.LookAhead(partial1 ++ partial2, eof)
      }
    }
  }

  private def convertLookAhead(closureToId: Map[Closure, Int], action: TmpAction): Action =
    action match {
      case TmpAction.Accept              => Action.Accept
      case TmpAction.Reduce(prod)        => Action.Reduce(prod.reducesTo.nt, prod.reducesTo.idx)
      case TmpAction.Shift(to)           => Action.Shift(closureToId(to))
      case TmpAction.LookAhead(onT, onE) =>
        Action.LookAhead(
          onT.map { case (t, a) => t -> convertLookAhead(closureToId, a) },
          onE.map(convertLookAhead(closureToId, _)),
        )
    }

}
