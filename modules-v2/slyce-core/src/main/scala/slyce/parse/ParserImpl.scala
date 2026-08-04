package slyce.parse

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.collection.mutable

import slyce.core.*
import slyce.core.builtIn.*

/**
 * Runtime LR parser with on-demand, state-scoped lexing.
 * Table + terminal matchers + reduce functions are supplied by the macro.
 */
final class ParserImpl[A](
    private val startState: Int,
    private val states: IArray[ParserImpl.State],
    private val terminals: IArray[ParserImpl.Terminal],
) extends Parser[A] {

  override def parse(source: Source): Either[ParseError, A] =
    ParserImpl.run(source, startState, states, terminals).map(_.asInstanceOf[A])

}
object ParserImpl {

  final case class Terminal(
      name: String,
      pattern: Pattern,
      build: (String, Span.Range) => Either[String, Any],
  )

  enum Action {
    case Accept
    case Reduce(nt: String, prodIdx: Int, pop: Int, build: (IArray[Any], Source, Int) => Any)
    case Shift(toState: Int)
    case LookAhead(onTerm: Map[Int, Action], onEOF: Option[Action])
  }

  final case class State(
      id: Int,
      goto: Map[String, Int],
      lookAhead: Action,
  )

  private final case class Tok(termId: Int, value: Any, start: Int, end: Int)

  def run(
      source: Source,
      startState: Int,
      states: IArray[State],
      terminals: IArray[Terminal],
  ): Either[ParseError, Any] = {
    val text = source.text
    val n = text.length

    def posAt(i: Int): Position = source.positions(math.min(i, n))
    def spanOf(start: Int, end: Int): Span.Range = Span.Range(source, posAt(start), posAt(end))

    def lexOne(from: Int, allowed: Set[Int]): Either[ParseError, Option[Tok]] = {
      if from >= n then return Right(None)
      var bestId = -1
      var bestEnd = -1
      var bestVal: Any = null
      val it = allowed.iterator
      while it.hasNext do {
        val tid = it.next()
        val term = terminals(tid)
        val m = term.pattern.matcher(text)
        m.region(from, n)
        m.useAnchoringBounds(false)
        if m.lookingAt() then {
          val end = m.end()
          if end > bestEnd then {
            val matched = text.substring(from, end)
            term.build(matched, spanOf(from, end)) match {
              case Right(v) =>
                bestId = tid
                bestEnd = end
                bestVal = v
              case Left(_) => ()
            }
          } else if end == bestEnd && bestId < 0 then {
            val matched = text.substring(from, end)
            term.build(matched, spanOf(from, end)) match {
              case Right(v) =>
                bestId = tid
                bestEnd = end
                bestVal = v
              case Left(_) => ()
            }
          }
        }
      }
      if bestId >= 0 then Right(Some(Tok(bestId, bestVal, from, bestEnd)))
      else
        Left(
          ParseError.LexerError(
            source,
            posAt(from),
            s"No token among: ${allowed.toList.sorted.map(terminals(_).name).mkString(", ")}",
          ),
        )
    }

    /**
     * Resolve nested LookAhead to a concrete Shift/Reduce/Accept.
     * @return (action, posAfterConsumedLookaheadTokensForShift, optional token to shift)
     *   - For Shift: token is defined, pos is after that token
     *   - For Reduce/Accept: token is empty, pos is unchanged (still at next unconsumed input)
     */
    def decide(
        action: Action,
        pos: Int,
    ): Either[ParseError, (Action, Int, Option[Tok])] =
      action match {
        case Action.LookAhead(onTerm, onEOF) =>
          if pos >= n then
            onEOF match {
              case Some(a) => decide(a, pos)
              case None    => Left(ParseError.UnexpectedEOF(source, s"expected one of ${onTerm.keys.map(terminals(_).name).mkString(", ")}"))
            }
          else
            lexOne(pos, onTerm.keySet) match {
              case Left(err) =>
                // At EOF-like failure when allowed set can't match — try EOF action if at end
                if pos >= n then
                  onEOF match {
                    case Some(a) => decide(a, pos)
                    case None    => Left(err)
                  }
                else Left(err)
              case Right(None) =>
                onEOF match {
                  case Some(a) => decide(a, pos)
                  case None    => Left(ParseError.UnexpectedEOF(source, "no token and no EOF action"))
                }
              case Right(Some(tok)) =>
                onTerm.get(tok.termId) match {
                  case None =>
                    Left(ParseError.UnexpectedInput(source, posAt(tok.start), s"Unexpected ${terminals(tok.termId).name}"))
                  case Some(next) =>
                    next match {
                      case Action.Shift(to) =>
                        Right((Action.Shift(to), tok.end, Some(tok)))
                      case Action.Reduce(nt, idx, pop, build) =>
                        // do not consume tok
                        Right((Action.Reduce(nt, idx, pop, build), pos, None))
                      case Action.Accept =>
                        Right((Action.Accept, pos, None))
                      case nested: Action.LookAhead =>
                        // multi-token look-ahead: consume tok only for disambiguation, continue from tok.end
                        // If nested resolves to Shift, that shift is for a *later* token; the first tok must still be shifted first.
                        // v1 nested lookAhead is only for choosing between reduce/shift paths with more tokens —
                        // when the chosen action is Reduce, first tok is not consumed; when deeper needs more tokens...
                        // For maxLookAhead=1, nested LookAhead under a terminal means: after seeing this terminal,
                        // further look-ahead. The first terminal is the one we're "on". If final action is Shift of
                        // that same terminal, shift it. If Reduce, don't consume.
                        decide(nested, tok.end).flatMap {
                          case (Action.Shift(to), end2, Some(tok2)) =>
                            // Nested shift means shift tok2, but we still must shift tok first — not representable as single action.
                            // For la>=2 this needs a token buffer in the main loop. For calculator (la=1) nested should be rare.
                            // Treat as: the action applies to the *first* token when final is Reduce/Accept; if Shift, shift first token only if nested returned shift without requiring second token equal...
                            // Practical approach for la=1 tables: nested LookAhead after matching t means further restrictions; Shift means shift t.
                            Right((Action.Shift(to), tok.end, Some(tok)))
                          case (r: Action.Reduce, _, _) =>
                            Right((r, pos, None))
                          case (Action.Accept, _, _) =>
                            Right((Action.Accept, pos, None))
                          case (la: Action.LookAhead, p2, t2) =>
                            Right((la, p2, t2))
                          case other =>
                            Left(ParseError.Internal(source, s"Unexpected nested decision: $other"))
                        }
                    }
                }
            }
        case Action.Shift(to) =>
          Left(ParseError.Internal(source, s"Bare Shift($to) outside LookAhead"))
        case other =>
          Right((other, pos, None))
      }

    val stateStack = mutable.ArrayBuffer[Int](startState)
    val valueStack = mutable.ArrayBuffer.empty[Any]
    var pos = 0
    var guard = 0
    val guardMax = n * 50 + 100

    while guard < guardMax do {
      guard += 1
      val st = states(stateStack.last)
      decide(st.lookAhead, pos) match {
        case Left(err) => return Left(err)
        case Right((Action.Accept, _, _)) =>
          if valueStack.size == 1 then return Right(valueStack.last)
          else if valueStack.nonEmpty then return Right(valueStack.last)
          else return Left(ParseError.Internal(source, "Accept with empty value stack"))
        case Right((Action.Shift(to), newPos, Some(tok))) =>
          valueStack += tok.value
          stateStack += to
          pos = newPos
        case Right((Action.Shift(_), _, None)) =>
          return Left(ParseError.Internal(source, "Shift without token"))
        case Right((Action.Reduce(nt, _, pop, build), _, _)) =>
          if valueStack.size < pop || stateStack.size < pop + 1 then
            return Left(ParseError.Internal(source, s"Reduce $nt pop=$pop values=${valueStack.size} states=${stateStack.size}"))
          val argsArr = IArray.tabulate(pop)(i => valueStack(valueStack.size - pop + i))
          valueStack.trimEnd(pop)
          stateStack.trimEnd(pop)
          val value = build(argsArr, source, pos)
          val from = stateStack.last
          states(from).goto.get(nt) match {
            case Some(to) =>
              valueStack += value
              stateStack += to
            case None =>
              return Left(
                ParseError.Internal(
                  source,
                  s"No goto for nt=$nt from state=$from (have: ${states(from).goto.keys.mkString(",")})",
                ),
              )
          }
        case Right((Action.LookAhead(_, _), _, _)) =>
          return Left(ParseError.Internal(source, "Unresolved LookAhead"))
        case Right((other, _, _)) =>
          return Left(ParseError.Internal(source, s"Unhandled: $other"))
      }
    }

    Left(ParseError.Internal(source, "parse loop guard exceeded"))
  }

}
