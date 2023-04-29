package slyce.parse

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import harness.core.*
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

    final case class Yield[Tok] private (
        span: Option[(Int, Int)],
        build: Span.Highlight => PartialFunction[String, Tok],
    )
    object Yield {
      def apply[Tok](
          span: (Int, Int),
          build: Span.Highlight => PartialFunction[String, Tok],
      ): Yield[Tok] =
        new Yield[Tok](
          Option.when(span != (0, -1))(span),
          build,
        )
    }

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
  object State {

    def fromMap[Tok](
        id: Int,
        on: => Map[Int, Option[State[Tok]]],
        elseOn: => Option[State[Tok]],
        yields: Option[Yields[Tok]],
    ): State[Tok] = {
      lazy val lazyOn: Map[Int, Option[State[Tok]]] = on

      State(
        id = id,
        on = c => lazyOn.getOrElse(c.toInt, elseOn),
        yields = yields,
      )
    }

    def fromPF[Tok](
        id: Int,
        yields: Option[Yields[Tok]],
    )(
        pf: PartialFunction[Int, Option[State[Tok]]],
    ): State[Tok] =
      State(
        id = id,
        on = c => pf.lift(c.toInt).flatten,
        yields = yields,
      )

  }

  object tokenize {

    // TODO (KR) : Make this a normal function if it ends up making sense
    private[Lexer] def apply[Tok <: Token](state0: State[Tok])(source: Source): Validated[List[Tok]] =
      loop(source, state0, state0, Nil, Span.Pos.Start, source.arr.lift, 0, Nil, Nil, None)

    private final case class Hit[Tok](
        yields: Yields[Tok],
        pos: Span.Pos,
        nextIdx: Int,
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
        chars: Int => Option[Char],
        idx: Int,
        rSeen: List[(Char, Span.Pos)],
        rToks: List[Tok],
        hit: Option[Hit[Tok]],
    ): Validated[List[Tok]] =
      chars(idx) match {
        case Some(head) =>
          state.on(head) match {
            case Some(to) =>
              val pair = (head, pos)
              val nPos = pos.onChar(head)
              val hit = to.yields.map { y => Hit(y, nPos, idx + 1, NonEmptyList(pair, rSeen)) }
              loop(source, to, mode, modeStack, nPos, chars, idx + 1, pair :: rSeen, rToks, hit)
            case None =>
              hit match {
                case Some(Hit(yields, pos, nextIdx, rHitChars)) =>
                  val markedChars = calcMarkedString(source, rHitChars)

                  (
                    nextState(mode, modeStack, yields.toMode, markedChars.span),
                    appendTokens(source, markedChars, yields.yields, rToks),
                  ).parTupled match {
                    case Right((nState, nModeStack), nRToks) => loop(source, nState, nState, nModeStack, pos, chars, nextIdx, Nil, nRToks, None)
                    case Left(errors)                        => errors.asLeft
                  }
                case None =>
                  Marked(s"Unexpected char : ${head.unesc}", Span.Highlight(pos, pos, source)).leftNel
              }
          }
        case None =>
          hit match {
            case Some(Hit(yields, _, _, rHitChars)) =>
              val markedChars = calcMarkedString(source, rHitChars)

              (
                nextState(mode, modeStack, yields.toMode, markedChars.span),
                appendTokens(source, markedChars, yields.yields, rToks),
              ).parTupled match {
                case Right(_, rToks) =>
                  rToks.reverse.asRight
                case Left(errors) => errors.asLeft
              }
            case None =>
              rToks.reverse.asRight
          }
      }

    private def getTextAndSpan(
        source: Source,
        subStr: Option[(Int, Int)],
        str: String,
        markedChars: Marked[Array[(Char, Span.Pos)]],
    ): Validated[(String, Span.Highlight)] =
      subStr match {
        case None =>
          (str, Span.Highlight(markedChars.value.head._2, markedChars.value.last._2, source)).asRight
        case Some((min, max)) =>
          def idx(i: Int): Validated[Int] = {
            val i2 = if (i >= 0) i else i + str.length
            if (i2 >= 0 && i2 <= str.length) i2.asRight
            else badLexer(s"Invalid substring bounds ($i)", markedChars.span).leftNel
          }

          (idx(min), idx(max)).parTupled.flatMap { (min, max) =>
            if (min <= max) {
              val text = str.substring(min, max + 1)
              val span = Span.Highlight(markedChars.value(min)._2, markedChars.value(max)._2, source)
              (text, span).asRight
            } else badLexer("Invalid substring bounds (min > max)", markedChars.span).leftNel
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
        case Yields.Yield(subStr, build) :: tail =>
          getTextAndSpan(source, subStr, str, markedChars).flatMap { (text, span) =>
            build(span).lift(text) match {
              case Some(tok) => tok.asRight
              case None      => badLexer(s"Invalid raw-terminal text : ${text.unesc}", span).leftNel
            }
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
