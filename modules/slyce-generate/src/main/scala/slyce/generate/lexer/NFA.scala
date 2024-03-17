package slyce.generate.lexer

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import harness.core.InfiniteSet
import java.util.UUID
import scala.util.hashing.MurmurHash3

import slyce.core.*
import slyce.generate.*
import slyce.generate.lexer.Regex.*

final case class NFA private (
    startMode: Marked[String],
    modes: Map[String, Marked[Pointer[NFA.State]]],
)
object NFA {

  sealed trait State
  object State {
    sealed trait NonTrivial extends State

    final case class TransitionOnChars(charClass: CharClass, to: Pointer[State]) extends State.NonTrivial
    final case class TransitionOnEpsilon(to: List[Pointer[State]]) extends State with Helpers.ExactEquality
    final case class End(line: LexerInput.Mode.Line) extends State.NonTrivial
  }

  // TODO (KR) : Mark errors more granularly
  object fromLexer {

    def apply(lexer: LexerInput): Validated[NFA] =
      Validated.withValidations(
        validateNonEmptyModes(lexer),
      ) {
        lexer.modes
          .parTraverse { mode =>
            modeToState(mode).map { state =>
              (mode.name.value, mode.name.as(state))
            }
          }
          .map { list =>
            NFA(lexer.startMode, list.toMap)
          }
      }

    private def validateNonEmptyModes(lexer: LexerInput): Validated[Any] =
      lexer.modes.parTraverse { mode =>
        if (mode.lines.nonEmpty) ().rightNel
        else mode.name.as(s"Mode has no lines : ${mode.name.value}").leftNel
      }

    private def modeToState(mode: LexerInput.Mode): Validated[Pointer[State]] =
      mode.lines.parTraverse { line => regexToState(line.regex.span, line.regex.value, Pointer(State.End(line))) }.map(stateWithEpsilonsTo(_*))

    private def stateWithEpsilonsTo(states: Pointer[State]*): Pointer[State] =
      Pointer(State.TransitionOnEpsilon(states.toList))

    private def regexToState(lineSpan: Span, reg: Regex, next: Pointer[State]): Validated[Pointer[State]] =
      reg match {
        case cc: CharClass =>
          cc.chars match {
            case InfiniteSet.Inclusive(chars) if chars.isEmpty => Marked("Can not transition on an empty Inclusive set", lineSpan).leftNel
            case _                                             => Pointer(State.TransitionOnChars(cc, next)).asRight
          }
        case Sequence(seq)         => seq.foldRight(next.rightNel[Marked[String]]) { (reg, vNext) => vNext.flatMap(regexToState(lineSpan, reg, _)) }
        case Group(seqs)           => seqs.toList.parTraverse(regexToState(lineSpan, _, next)).map(stateWithEpsilonsTo(_*))
        case Repeat(reg, min, max) => repeatToState(lineSpan, reg, min, max, next)
      }

    private object repeatToState {

      def apply(lineSpan: Span, reg: Regex, min: Int, max: Option[Int], next: Pointer[State]): Validated[Pointer[State]] =
        Validated.withValidations(
          validateMin(lineSpan, min),
          validateMax(lineSpan, min, max),
        ) {
          max match {
            case Some(max) =>
              doRegexOrSkip(lineSpan, reg, max - min, next).flatMap { afterMin =>
                repeat(lineSpan, reg, min, afterMin)
              }
            case None =>
              loopOnSelf(lineSpan, reg, next).flatMap { loopOnSelf =>
                repeat(lineSpan, reg, min, loopOnSelf)
              }
          }
        }

      private def validateMin(lineSpan: Span, min: Int): Validated[Unit] =
        if (min < 0) Marked(s"min($min) < 0", lineSpan).leftNel
        else ().asRight

      private def validateMax(lineSpan: Span, min: Int, max: Option[Int]): Validated[Unit] =
        max match {
          case Some(max) if max < min              => Marked(s"max($max) < min($min)", lineSpan).leftNel
          case Some(max) if min == max && min == 0 => Marked(s"max($max) == min($min) == 0", lineSpan).leftNel
          case _                                   => ().asRight
        }

      private def repeat(lineSpan: Span, reg: Regex, times: Int, next: Pointer[State]): Validated[Pointer[State]] =
        if (times > 0) regexToState(lineSpan, reg, next).flatMap(repeat(lineSpan, reg, times - 1, _))
        else next.asRight

      private def loopOnSelf(lineSpan: Span, reg: Regex, next: Pointer[State]): Validated[Pointer[State]] =
        Pointer.withSelfWrapped[State, Validated] { self =>
          regexToState(lineSpan, reg, self).map { state =>
            stateWithEpsilonsTo(state, next)
          }
        }

      private def doRegexOrSkip(lineSpan: Span, reg: Regex, times: Int, next: Pointer[State]): Validated[Pointer[State]] =
        if (times > 0)
          regexToState(lineSpan, reg, next).flatMap { _next =>
            doRegexOrSkip(
              lineSpan,
              reg,
              times - 1,
              Pointer(State.TransitionOnEpsilon(next :: _next :: Nil)),
            )
          }
        else
          next.asRight

    }

  }

}
