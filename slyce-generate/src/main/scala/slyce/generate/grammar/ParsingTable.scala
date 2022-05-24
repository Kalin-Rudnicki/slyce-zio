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
import slyce.generate.grammar.ExpandedGrammar.Identifier

final case class ParsingTable private (
    // TODO (KR) :
)
object ParsingTable {

  // TODO (KR) : Make configurable
  val MaxLookAhead: Int = 1

  object fromExpandedGrammar {

    // TODO (KR) : Remove all 'println'
    def apply(expandedGrammar: ExpandedGrammar): Validated[ParsingTable] =
      Validated.withValidations(
        validateDefinedNTs(expandedGrammar.startNt, expandedGrammar.deDuplicatedNTGroups),
      ) {
        val productionsForNT: Map[ExpandedGrammar.Identifier.NonTerminal, List[Closure.Production]] =
          expandedGrammar.deDuplicatedNTGroups.flatMap {
            _.rawNTs.toList.map { nt =>
              (
                nt.name,
                nt.productions.toList.zipWithIndex.map { (prod, idx) =>
                  Closure.Production(ReducesTo.Production(nt.name, idx), prod.elements)
                },
              )
            }
          }.toMap

        val initialEntry: Closure.Entry =
          Closure.Entry(
            reducesTo = ReducesTo.###,
            seen = Nil,
            waiting = ExpandedGrammar.Identifier.NonTerminal.NamedNt(expandedGrammar.startNt.value) :: Nil,
            lookAhead = Follow(Set.empty, true) :: Nil,
          )

        val initialClosure: Closure = expandEntries(productionsForNT, Set(initialEntry))

        val allClosures: List[(Closure, Int)] =
          Helpers.findAll(Set(initialClosure)) { c => calcTransitionMap(productionsForNT, c).values.toSet }.toList.zipWithIndex

        val closureIdMap: Map[Closure, Int] = allClosures.toMap

        allClosures.foreach { (closure, id) =>
          val transitionMap = calcTransitionMap(productionsForNT, closure)
          val terminalsWithTransitions: Set[ExpandedGrammar.Identifier.Term] = transitionMap.keySet.collect { case t: ExpandedGrammar.Identifier.Term => t }
          val headFollowsForFinishedEntries: Set[Follow] = closure.finishedEntries.map(_.lookAhead.head)
          // TODO (KR) : Accurate?
          val conflicts = terminalsWithTransitions & headFollowsForFinishedEntries.flatMap(_.validTerminals)
          debugging.showEntries(id.toString, closure.entries)
          transitionMap.foreach { (id, to) => println(s"  >> $id -> ${closureIdMap(to)}") }
          conflicts.toList.sortBy(_.toString).foreach { t => println(s"      >>>> Shift/Reduce conflict : $t".redBg) }
          if (conflicts.nonEmpty) java.lang.System.exit(0)
        }

        println()
        println()
        println(s"Start state: ${closureIdMap(initialClosure)}")

        println()
        println()
        allClosures.foreach { (c: Closure, i) => // TODO (KR) :
          c.entries.groupBy(e => e.reducesTo).foreach { (k, v) =>
            if (v.size > 1) {
              println(s"  >> Closure#$i : $k <- ${v.size}")
              v.foreach { e => println(s"    >> [${e.seen.size}] ${e.lookAhead.mkString("  ")}") }
            }
          }
        }

        allClosures.parTraverse { (c, _) =>
          calcActionState(productionsForNT, c)
        } match {
          case Right(res) =>
            println("Success")
            println(s"Total unique closures: ${allClosures.size}")
            println(s"Total unique action-states: ${res.toList.toSet.size}")
          case Left(fails) => fails.toList.foreach(println(_))
        }

        // TODO (KR) :
        Validated.???
      }

    // =====| Types |=====

    private final case class Closure(entries: Set[Closure.Entry]) {
      lazy val (finishedEntries: Set[Closure.Entry.Finished], unfinishedEntries: Set[Closure.Entry.Waiting]) =
        entries.partitionMap {
          case e: Closure.Entry.Finished => e.asLeft
          case e: Closure.Entry.Waiting  => e.asRight
        }
    }
    private object Closure {

      final case class Production(
          reducesTo: ReducesTo.Production,
          producesIds: List[ExpandedGrammar.Identifier],
      )

      // TODO (KR) : I would like for this to be cleaned up a bit
      sealed trait Entry {
        val reducesTo: ReducesTo
        val seen: List[ExpandedGrammar.Identifier]
        val lookAhead: List[Follow]
        final lazy val waitingList: List[ExpandedGrammar.Identifier] =
          this match {
            case Entry.Waiting(_, _, waiting, _) => waiting.toList
            case _: Entry.Finished               => Nil
          }
      }
      object Entry {

        final case class Finished(
            reducesTo: ReducesTo,
            seen: List[ExpandedGrammar.Identifier],
            lookAhead: List[Follow],
        ) extends Entry

        final case class Waiting(
            reducesTo: ReducesTo,
            seen: List[ExpandedGrammar.Identifier],
            waiting: NonEmptyList[ExpandedGrammar.Identifier],
            lookAhead: List[Follow],
        ) extends Entry

        def apply(reducesTo: ReducesTo, seen: List[ExpandedGrammar.Identifier], waiting: List[ExpandedGrammar.Identifier], lookAhead: List[Follow]): Entry =
          waiting.toNel match {
            case Some(waiting) => Entry.Waiting(reducesTo, seen, waiting, lookAhead)
            case None          => Entry.Finished(reducesTo, seen, lookAhead)
          }

      }

    }

    private final case class Follow(
        validTerminals: Set[ExpandedGrammar.Identifier.Term],
        eofIsValid: Boolean,
    ) {
      override def toString: String =
        (validTerminals.map(_.toString).toList.sorted ::: Option.when(eofIsValid)("$".cyan.toString).toList)
          .mkString("Follow< ", ", ", " >")
          .magentaBg
          .toString
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

    private final case class ActionState(
        actionsOnNonTerminals: Map[ExpandedGrammar.Identifier.NonTerminal, ActionState.Action.Push],
        lookAhead: ActionState.Action.LookAhead,
    )
    private object ActionState {

      final case class Finished(
          reducesTo: ReducesTo,
          ids: List[ExpandedGrammar.Identifier],
          lookAhead: List[Follow],
      )

      sealed trait Action
      object Action {
        sealed trait EOFAction extends Action

        case object Accept extends Action.EOFAction
        final case class Reduce(production: Closure.Production) extends Action.EOFAction
        final case class Push(to: Closure) extends Action
        final case class LookAhead(
            actionsOnTerminals: Map[ExpandedGrammar.Identifier.Term, Action],
            actionOnEOF: Option[Action.EOFAction],
        ) extends Action
      }

    }

    // =====| Helpers |=====

    // TODO (KR) : Remove
    private object debugging {

      def showEntries(label: String, entries: Set[Closure.Entry], filter: PartialFunction[Closure.Entry, Closure.Entry] = identity(_)): Unit = {
        val filtered = entries.toList.collect(filter)

        println()
        println()
        println()
        println(s"=====| $label (${filtered.size} / ${entries.size}) |=====")
        println {
          filtered
            .sortBy(_.toString)
            .map { e =>
              IndentedString.inline(
                s">> ${e.reducesTo}${if (e.waitingList.isEmpty) " (Reduce)".red.whiteBg else ""}",
                IndentedString.indented(
                  s"     seen[${e.seen.size}]: ${e.seen.mkString(" , ".red.toString)}",
                  s"  waiting[${e.waitingList.size}]: ${e.waitingList.mkString(" , ".red.toString)}",
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
        productionsForNT: Map[ExpandedGrammar.Identifier.NonTerminal, List[Closure.Production]],
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
                  val inlined: List[List[(ReducesTo, Int, ExpandedGrammar.Identifier)]] = {
                    productionsForNT(nt).map { case Closure.Production(rt, waiting) =>
                      waiting.zipWithIndex.map { (i, idx) => (rt, idx, i) }
                    }
                  }
                  mergeFollows(inlined.map(ids => calcLookAhead(productionsForNT, ids ::: tail, newExpanded, ifPassThrough, maxLookAhead)))
                }
              case t: ExpandedGrammar.Identifier.Term =>
                Follow(Set(t), false) :: calcLookAhead(productionsForNT, tail, Set.empty, ifPassThrough, maxLookAhead - 1)
            }
        }

    private def expandEntries(
        productionsForNT: Map[ExpandedGrammar.Identifier.NonTerminal, List[Closure.Production]],
        initial: Set[Closure.Entry],
    ): Closure = {
      val preJoinedExpansion: Set[Closure.Entry] =
        Helpers.findAll(initial) {
          case Closure.Entry.Waiting(rt, seen, NonEmptyList(next: ExpandedGrammar.Identifier.NonTerminal, waiting), lookAhead) =>
            val lookup: List[Closure.Production] = productionsForNT(next)

            val newIds: List[(ReducesTo, Int, ExpandedGrammar.Identifier)] =
              waiting.zipWithIndex.map { (id, idx) => (rt, seen.size + 1 + idx, id) }

            val newFollows: List[Follow] =
              calcLookAhead(productionsForNT, newIds, Set.empty, lookAhead, MaxLookAhead)

            lookup.map { case Closure.Production(prod, waiting) =>
              Closure.Entry(prod, Nil, waiting, newFollows)
            }.toSet
          case _ =>
            Set.empty
        }

      val joinedExpansion: Set[Closure.Entry] =
        preJoinedExpansion
          .groupMap(e => (e.reducesTo, e.seen, e.waitingList))(_.lookAhead)
          .toSet
          .map { case ((rt, seen, waiting), follows) =>
            Closure.Entry(rt, seen, waiting, mergeFollows(follows.toList))
          }

      Closure(joinedExpansion)
    }

    private def calcTransitionMap(
        productionsForNT: Map[ExpandedGrammar.Identifier.NonTerminal, List[Closure.Production]],
        closure: Closure,
    ): Map[ExpandedGrammar.Identifier, Closure] =
      closure.unfinishedEntries
        .map { case Closure.Entry.Waiting(rt, seen, NonEmptyList(next, stillWaiting), lookAhead) =>
          (next, Closure.Entry(rt, seen :+ next, stillWaiting, lookAhead))
        }
        .groupMap(_._1)(_._2)
        .map { (id, entries) => (id, expandEntries(productionsForNT, entries)) }

    private def calcActionState(
        productionsForNT: Map[ExpandedGrammar.Identifier.NonTerminal, List[Closure.Production]],
        closure: Closure,
    ): Validated[ActionState] = {
      val (
        ntTransitionMap: Map[ExpandedGrammar.Identifier.NonTerminal, ActionState.Action.Push],
        terminalTransitionMap: Map[ExpandedGrammar.Identifier.Term, (Closure, List[Follow])],
      ) = calcSplitTransitionMaps(productionsForNT, closure)

      calcTerminalActions(terminalTransitionMap, closure.finishedEntries)
        .map(ActionState(ntTransitionMap, _))
    }

    private def calcSplitTransitionMaps(
        productionsForNT: Map[ExpandedGrammar.Identifier.NonTerminal, List[Closure.Production]],
        closure: Closure,
    ): (
        Map[ExpandedGrammar.Identifier.NonTerminal, ActionState.Action.Push],
        Map[ExpandedGrammar.Identifier.Term, (Closure, List[Follow])],
    ) = {
      val (ntList, tList) =
        calcTransitionMap(productionsForNT, closure).partitionMap {
          case (nt: ExpandedGrammar.Identifier.NonTerminal, c) => (nt, ActionState.Action.Push(c)).asLeft
          case (t: ExpandedGrammar.Identifier.Term, c)         => (t, (c, calcFollowsForClosure(productionsForNT, c))).asRight
        }

      (ntList.toMap, tList.toMap)
    }

    private def calcFollowsForClosure(
        productionsForNT: Map[ExpandedGrammar.Identifier.NonTerminal, List[Closure.Production]],
        c: Closure,
    ): List[Follow] =
      mergeFollows(
        c.entries.toList.map { e =>
          calcLookAhead(
            productionsForNT,
            e.waitingList.zipWithIndex.map { (id, idx) =>
              (e.reducesTo, e.seen.size + idx, id)
            },
            Set.empty,
            e.lookAhead,
            MaxLookAhead,
          )
        },
      )

    // TODO (KR) : It might make sense to create separate types for '(Closure, List[Follow])' and/or 'Set[Closure.Entry.Finished]'.
    //           : The reason for this is that both have to do with advancing the 'follows',
    //           : and the way that it is re-using types just does not seem very clear or straight-forward.
    private def calcTerminalActions(
        terminalTransitionMap: Map[ExpandedGrammar.Identifier.Term, (Closure, List[Follow])],
        finishedEntries: Set[Closure.Entry.Finished], // NOTE : These have their 'follow' already adjusted
    ): Validated[ActionState.Action.LookAhead] = {
      val (
        fesWithoutLookAhead: Set[Closure.Entry.Finished],
        fesWithLookAhead: Set[(Follow, Closure.Entry.Finished)],
      ) =
        finishedEntries.partitionMap { e =>
          e.lookAhead.toNel match {
            case Some(NonEmptyList(head, tail)) => (head, Closure.Entry.Finished(e.reducesTo, e.seen, tail)).asRight
            case None                           => e.asLeft
          }
        }

      if (fesWithoutLookAhead.nonEmpty) Marked(s"No more look-ahead to use, consider increasing max-look-ahead: ${fesWithoutLookAhead.map(_.reducesTo).mkString(", ")}", Span.Unknown).leftNel
      else if (fesWithLookAhead.isEmpty) {
        val actionsOnTerminals = terminalTransitionMap.map { case (t, (c, _)) => (t, ActionState.Action.Push(c)) }
        ActionState.Action.LookAhead(actionsOnTerminals, None).asRight
      } else {
        val fesWithTransitionOnTerminal: Map[ExpandedGrammar.Identifier.Term, Set[Closure.Entry.Finished]] =
          fesWithLookAhead
            .flatMap { (follow, finished) =>
              follow.validTerminals.toList.map((_, finished))
            }
            .groupMap(_._1)(_._2)

        val fesWithEOFLookAhead: Set[Closure.Entry.Finished] =
          fesWithLookAhead.collect { case (Follow(_, true), finish) => finish }

        val actionOnEOF: Validated[Option[ActionState.Action.EOFAction]] =
          fesWithEOFLookAhead.toList match {
            case Nil => None.asRight
            case value :: Nil =>
              value.reducesTo match {
                case ReducesTo.###            => ActionState.Action.Accept.some.asRight
                case rt: ReducesTo.Production => ActionState.Action.Reduce(Closure.Production(rt, value.seen)).some.asRight
              }
            case values => Marked(s"Multiple EOF actions: ${values.map(_.reducesTo).mkString(", ")}", Span.Unknown).leftNel
          }

        val partialActionsOnTerminals1: Validated[Map[ExpandedGrammar.Identifier.Term, ActionState.Action]] =
          fesWithTransitionOnTerminal.toList
            .parTraverse { (t, fes) =>
              (fes.toList, terminalTransitionMap.get(t)) match {
                case (Nil, Some((c, _))) => (t, ActionState.Action.Push(c)).asRight
                case (fe :: Nil, None) =>
                  fe.reducesTo match {
                    case prod: ReducesTo.Production => (t, ActionState.Action.Reduce(Closure.Production(prod, fe.seen))).asRight
                    case ReducesTo.###              => Marked("I don't think this should be possible... (reduce to ###)", Span.Unknown).leftNel
                  }
                case (fes, None) => calcTerminalActions(Map.empty, fes.toSet).map((t, _))
                case (fes, Some((c, cfs))) =>
                  cfs.toNel match {
                    case Some(NonEmptyList(head, tail)) => calcTerminalActions(head.validTerminals.toList.map((_, (c, tail))).toMap, fes.toSet).map((t, _))
                    case None                           => Marked(s"No more look-ahead for: $c", Span.Unknown).leftNel
                  }
              }
            }
            .map(_.toMap)

        (partialActionsOnTerminals1, actionOnEOF).parMapN { (partialActionsOnTerminals1, actionOnEOF) =>
          val terminalsReferencedByFinishedEntries: Set[Identifier.Term] = fesWithLookAhead.flatMap(_._1.validTerminals)

          // Any terminals referenced in the look-ahead of 'fesWithLookAhead' would end up in 'partialActionsOnTerminals1'
          val partialActionsOnTerminals2: Map[ExpandedGrammar.Identifier.Term, ActionState.Action] =
            terminalTransitionMap
              .filterNot { (t, _) => terminalsReferencedByFinishedEntries.contains(t) }
              .map { case (t, (c, _)) => (t, ActionState.Action.Push(c)) }

          ActionState.Action.LookAhead(partialActionsOnTerminals1 ++ partialActionsOnTerminals2, actionOnEOF)
        }
      }
    }

  }
}
