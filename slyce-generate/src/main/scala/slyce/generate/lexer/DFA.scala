package slyce.generate.lexer

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.list.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import klib.utils.InfiniteSet
import scala.annotation.tailrec

import slyce.core.*
import slyce.generate.*

final case class DFA private (
    modeStarts: Map[String, DFA.State], // Not actually used, just for reference
    states: List[DFA.State],
)
object DFA {

  final case class State(
      id: Int,
      transitions: Map[Set[Char], Option[Lazy[State]]],
      elseTransition: Option[Lazy[State]],
      yields: Option[(LexerInput.Mode.Line, Yields[Lazy[State]])],
  )

  object fromNFA {

    def apply(nfa: NFA): Validated[DFA] =
      Validated.withValidations(
        validateModeNames(nfa),
      ) {
        val preModeList: List[(String, NFAStates, IState, Set[Shadows])] =
          nfa.modes.toList.map { (name, state) =>
            val expanded = expandEpsilons(Set(state.value.value))
            val (iState, shadows) = nfaStatesToIState(expanded)
            (name, expanded, iState, shadows)
          }

        val modeMap: Map[String, (NFAStates, IState)] =
          preModeList.map { (name, nfaStates, iState, _) =>
            name -> (nfaStates, iState)
          }.toMap

        val shadows1: Set[Shadows] = preModeList.flatMap(_._4).toSet

        val (iStateMap: Map[NFAStates, IState], shadows2: Set[Shadows]) = findAllIStatesAndShadows(modeMap)

        val startingStates: NFAStates = modeMap(nfa.startMode.value)._1

        val sortedPairs: List[(NFAStates, IState, Int)] = iStateMap.toList.sortBy(_._1 != startingStates).zipWithIndex.map { case ((nfaStates, iState), i) => (nfaStates, iState, i) }

        val nfaStatesToState: Map[NFAStates, State] = buildStateMap(sortedPairs, modeMap)

        val stateModeStarts: Map[String, State] = buildStateModeStarts(nfa, nfaStatesToState)

        val allStates: List[State] = nfaStatesToState.values.toList.sortBy(_.id)

        Validated.withValidations(
          validateModeStartStateCanNotYield(stateModeStarts),
          validateShadows(allStates, shadows1 ++ shadows2),
        ) {
          DFA(stateModeStarts, allStates).asRight
        }
      }

    private type NFAStates = Set[NFA.State.NonTrivial]

    private final case class IState(
        transitions: Map[Set[Char], Option[NFAStates]],
        elseTransition: Option[NFAStates],
        yieldLine: Option[LexerInput.Mode.Line],
    )

    private final case class Shadows(
        selectedLine: LexerInput.Mode.Line,
        shadowedLine: LexerInput.Mode.Line,
    )

    private def expandEpsilons(states: Set[NFA.State]): NFAStates =
      Helpers
        .findAll(states) {
          case nonTrivial: NFA.State.NonTrivial  => Set(nonTrivial)
          case NFA.State.TransitionOnEpsilon(to) => to.map(_.value)
        }
        .collect { case nonTrivial: NFA.State.NonTrivial => nonTrivial }

    private def validateModeNames(nfa: NFA): Validated[Any] = {
      val allNFAStates: Set[NFA.State] =
        Helpers.findAll(nfa.modes.values.map(_.value.value).toSet) {
          case NFA.State.TransitionOnChars(_, to) => Set(to.value)
          case NFA.State.End(_)                   => Set.empty
          case NFA.State.TransitionOnEpsilon(to)  => to.map(_.value)
        }

      val toModeNames =
        allNFAStates.toList.flatMap {
          case NFA.State.End(line) =>
            line.yields.toMode.value match {
              case Yields.ToMode.To(mode)   => Marked(mode, line.yields.toMode.span).some
              case Yields.ToMode.Push(mode) => Marked(mode, line.yields.toMode.span).some
              case _                        => None
            }
          case _ =>
            None
        }

      (nfa.startMode :: toModeNames).parTraverse { modeName =>
        if (nfa.modes.contains(modeName.value)) ().asRight
        else modeName.as(s"Invalid mode name : ${modeName.value}").leftNel
      }
    }

    private def nfaStatesToIState(expandedStates: NFAStates): (IState, Set[Shadows]) = {
      val transitionPairs: List[(InfiniteSet[Char], NFA.State)] =
        expandedStates.toList.collect { case NFA.State.TransitionOnChars(charClass, to) => (charClass.chars, to.value) }

      val explicitChars: Set[Char] =
        InfiniteSet.explicit(transitionPairs.map(_._1)*)

      val transitions: Map[Set[Char], Option[NFAStates]] =
        explicitChars.toList
          .map { char =>
            char ->
              expandEpsilons(
                transitionPairs
                  .filter(_._1.contains(char))
                  .map(_._2)
                  .toSet,
              ).someWhen(_.nonEmpty)
          }
          .groupMap(_._2)(_._1)
          .map { (k, v) => v.toSet -> k }

      val elseTransition: Option[NFAStates] =
        transitionPairs
          .collect { case (_: InfiniteSet.Exclusive[_], state) => state }
          .toSet
          .someWhen(_.nonEmpty)
          .map(expandEpsilons)

      val yieldLine: Option[(LexerInput.Mode.Line, Set[Shadows])] =
        expandedStates.toList
          .collect { case NFA.State.End(line) => line }
          .sortBy(_.lineNo)
          .toNel
          .map { case NonEmptyList(head, tail) =>
            (
              head,
              tail.map { shadowed => Shadows(head, shadowed) }.toSet,
            )
          }

      (
        IState(transitions, elseTransition, yieldLine.map(_._1)),
        yieldLine.fold(Set.empty[Shadows])(_._2),
      )
    }

    private object findAllIStatesAndShadows {

      def apply(modeMap: Map[String, (NFAStates, IState)]): (Map[NFAStates, IState], Set[Shadows]) =
        loop(
          Map.empty,
          modeMap.map(_._2),
          Set.empty,
        )

      @tailrec
      private def loop(
          seen: Map[NFAStates, IState],
          unseen: Map[NFAStates, IState],
          shadows: Set[Shadows],
      ): (Map[NFAStates, IState], Set[Shadows]) =
        if (unseen.isEmpty) (seen, shadows)
        else {
          val newSeen: Map[NFAStates, IState] = seen ++ unseen
          val newUnseen: Set[NFAStates] = unseen.flatMap { (_, iState) => childrenOfIState(iState) }.toSet &~ newSeen.keySet
          val converted: List[(NFAStates, IState, Set[Shadows])] =
            newUnseen.toList.map { s =>
              val (iState, shadows) = nfaStatesToIState(s)
              (s, iState, shadows)
            }

          loop(
            newSeen,
            converted.map { (nfaStates, iState, _) => nfaStates -> iState }.toMap,
            shadows ++ converted.flatMap(_._3).toSet,
          )
        }

      private def childrenOfIState(iState: IState): Set[NFAStates] =
        (iState.elseTransition.toList ::: iState.transitions.toList.flatMap(_._2)).toSet

    }

    private def buildStateMap(sortedIStates: List[(NFAStates, IState, Int)], modeMap: Map[String, (NFAStates, IState)]): Map[NFAStates, State] =
      Lazy.selfMap[(NFAStates, IState, Int), NFAStates, State](sortedIStates) { case ((nfaStates, iState, i), ef) =>
        nfaStates ->
          State(
            id = i,
            transitions = iState.transitions.map { (chars, to) => chars -> to.map(ef) },
            elseTransition = iState.elseTransition.map(ef),
            yields = iState.yieldLine.map { line => (line, line.yields.map { modeName => ef(modeMap(modeName)._1) }) },
          )
      }

    private def buildStateModeStarts(nfa: NFA, nfaStatesToState: Map[NFAStates, State]): Map[String, State] =
      nfa.modes.map { (name, state) =>
        name -> nfaStatesToState(expandEpsilons(Set(state.value.value)))
      }

    private def validateModeStartStateCanNotYield(stateModeStarts: Map[String, State]): Validated[Any] =
      stateModeStarts.toList.parTraverse { (name, state) =>
        if (state.yields.nonEmpty) Marked(s"Mode '$name' can yield on no input (at least 1 char is required before yielding)", Span.Unknown).leftNel
        else ().rightNel
      }

    private def validateShadows(allStates: List[State], shadows: Set[Shadows]): Validated[Any] = {
      val selectedLines: Set[LexerInput.Mode.Line] =
        allStates.flatMap(_.yields.map(_._1)).toSet

      val shadowMap: Map[LexerInput.Mode.Line, Set[LexerInput.Mode.Line]] =
        shadows.groupMap(_.shadowedLine)(_.selectedLine).filterNot { (shadowed, _) => selectedLines.contains(shadowed) }

      val errors: List[Marked[String]] =
        shadowMap.toList.map { (shadowed, selected) =>
          shadowed.regex.as(s"Line ${shadowed.lineNo} is completely shadowed by line(s): ${selected.map(_.lineNo).toList.sorted.mkString(", ")}")
        }

      errors.toNel.toLeft(())
    }

  }

}
