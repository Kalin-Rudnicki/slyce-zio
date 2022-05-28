package slyce.parse

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import klib.utils.*
import scala.annotation.tailrec

import slyce.core.*

final class Lexer[Tok <: Token](state0: Lexer.State[Tok]) {
  def tokenize(source: Source): Validated[List[Tok]] = Lexer.tokenize(state0)(source)
}
object Lexer {

  final case class Yields[Tok](
      yields: List[Yields.Yield[Tok]],
      toMode: Yields.ToMode[Tok],
  )
  object Yields {

    final case class Yield[Tok](
        span: (Option[Int], Option[Int]),
        build: (String, Span.Highlight) => Validated[Tok],
    )

    sealed trait ToMode[+Tok]
    object ToMode {
      case object Same extends ToMode[Nothing]
      final case class To[Tok](state: Lazy[State[Tok]]) extends ToMode[Tok]
      final case class Push[Tok](state: Lazy[State[Tok]]) extends ToMode[Tok]
      case object Pop extends ToMode[Nothing]
    }

  }

  final case class State[Tok](
      id: Int,
      on: Char => Option[State[Tok]],
      yields: Option[Yields[Tok]],
  )

  object tokenize {

    // TODO (KR) : Make this a normal function if it ends up making sense
    private[Lexer] def apply[Tok <: Token](state0: State[Tok])(source: Source): Validated[List[Tok]] =
      loop(source, state0, state0, Nil, Span.Pos.Start, source.chars, Nil, Nil, None)

    private final case class Hit[Tok](
        yields: Yields[Tok],
        pos: Span.Pos,
        remainingChars: List[Char],
        rHitChars: NonEmptyList[(Char, Span.Pos)],
    )

    private def badLexer(msg: String, span: Span): Marked[String] = Marked(s"Incorrectly designed lexer: $msg", span)

    @tailrec
    private def loop[Tok](
        source: Source,
        state: State[Tok],
        mode: State[Tok],
        modeStack: List[State[Tok]],
        pos: Span.Pos,
        chars: List[Char],
        rSeen: List[(Char, Span.Pos)],
        rToks: List[Tok],
        hit: Option[Hit[Tok]],
    ): Validated[List[Tok]] =
      chars match {
        case head :: tail =>
          state.on(head) match {
            case Some(to) =>
              val pair = (head, pos)
              val nPos = pos.onChar(head)
              val hit = to.yields.map { y => Hit(y, nPos, tail, NonEmptyList(pair, rSeen)) }
              loop(source, to, mode, modeStack, nPos, tail, pair :: rSeen, rToks, hit)
            case None =>
              hit match {
                case Some(Hit(yields, pos, remainingChars, rHitChars)) =>
                  val markedChars = calcMarkedString(source, rHitChars)

                  (
                    nextState(mode, modeStack, yields.toMode, markedChars.span),
                    appendTokens(source, markedChars, yields.yields, rToks),
                  ).parTupled match {
                    case Right((nState, nModeStack), nRToks) => loop(source, nState, nState, nModeStack, pos, remainingChars, Nil, nRToks, None)
                    case Left(errors)                        => errors.asLeft
                  }
                case None =>
                  Marked(s"Unexpected char : ${head.unesc}", Span.Highlight(pos, pos, source)).leftNel
              }
          }
        case Nil =>
          hit match {
            case Some(Hit(yields, _, _, rHitChars)) =>
              val markedChars = calcMarkedString(source, rHitChars)

              (
                nextState(mode, modeStack, yields.toMode, markedChars.span),
                appendTokens(source, markedChars, yields.yields, rToks),
              ).parTupled match {
                case Right(_, rToks) => rToks.reverse.asRight
                case Left(errors)    => errors.asLeft
              }
            case None =>
              rToks.reverse.asRight
          }
      }

    @tailrec
    private def appendTokens[Tok](
        source: Source,
        markedChars: Marked[Array[(Char, Span.Pos)]],
        yields: List[Yields.Yield[Tok]],
        toks: List[Tok],
    ): Validated[List[Tok]] = {
      val str: String = String(markedChars.value.map(_._1))
      yields match {
        case Yields.Yield((min, max), build) :: tail =>
          def idx(i: Int): Validated[Int] = {
            val i2 = if (i >= 0) i else i + str.length
            if (i2 >= 0 && i2 <= str.length) i2.asRight
            else badLexer(s"Invalid substring bounds ($i)", markedChars.span).leftNel
          }

          (idx(min.getOrElse(0)), idx(max.getOrElse(-1))).parTupled.flatMap { (min, max) =>
            if (min <= max) build(str.substring(min, max + 1), Span.Highlight(markedChars.value(min)._2, markedChars.value(max)._2, source))
            else badLexer("Invalid substring bounds (min > max)", markedChars.span).leftNel
          } match {
            case Right(tok)   => appendTokens[Tok](source, markedChars, tail, tok :: toks)
            case Left(errors) => errors.asLeft
          }
        case Nil => toks.asRight
      }
    }

    private def calcMarkedString(source: Source, rHitChars: NonEmptyList[(Char, Span.Pos)]): Marked[Array[(Char, Span.Pos)]] =
      Marked(rHitChars.toList.toArray.reverse, Span.Highlight(rHitChars.last._2, rHitChars.head._2, source))

    private def nextState[Tok](
        mode: State[Tok],
        modeStack: List[State[Tok]],
        toMode: Yields.ToMode[Tok],
        span: Span,
    ): Validated[(State[Tok], List[State[Tok]])] =
      toMode match {
        case Yields.ToMode.Same        => (mode, modeStack).asRight
        case Yields.ToMode.To(state)   => (state.value.asInstanceOf[State[Tok]], modeStack).asRight // TODO (KR) : compiler being a dumbass
        case Yields.ToMode.Push(state) => (state.value.asInstanceOf[State[Tok]], mode :: modeStack).asRight // TODO (KR) : compiler being a dumbass
        case Yields.ToMode.Pop =>
          modeStack match {
            case head :: tail => (head, tail).asRight
            case Nil          => badLexer("Attempted to pop modes with an empty mode-stack", span).leftNel
          }
      }

  }

}
