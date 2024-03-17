package slyce.parse

import cats.data.NonEmptyList
import cats.syntax.either.*
import scala.annotation.tailrec

import slyce.core.*
import slyce.parse.Grammar.State.Action

final class Grammar[Tok <: Token, NT <: NonTerminal, NTRoot <: NT](state0: Grammar.State[Tok, NT, NTRoot]) {
  def buildTree(source: Source, tokens: List[Tok]): Validated[NTRoot] = Grammar.buildTree(state0)(source, tokens)
}
object Grammar {

  final case class StackElement[Tok <: Token, NT <: NonTerminal, NTRoot <: NT](
      elem: Either[Tok, NT],
      state: State[Tok, NT, NTRoot],
  )

  final case class State[Tok <: Token, NT <: NonTerminal, NTRoot <: NT](
      id: Int,
      onTerm: PartialFunction[List[Tok], State.Action[Tok, NT, NTRoot]],
      onNT: PartialFunction[NT, State[Tok, NT, NTRoot]],
  )
  object State {

    sealed trait Action[Tok <: Token, NT <: NonTerminal, NTRoot <: NT]
    object Action {

      final case class Shift[Tok <: Token, NT <: NonTerminal, NTRoot <: NT](
          to: State[Tok, NT, NTRoot],
      ) extends Action[Tok, NT, NTRoot]

      final case class Reduce[Tok <: Token, NT <: NonTerminal, NTRoot <: NT](
          stackF: PartialFunction[(State[Tok, NT, NTRoot], List[StackElement[Tok, NT, NTRoot]]), (State[Tok, NT, NTRoot], NT, List[StackElement[Tok, NT, NTRoot]])],
      ) extends Action[Tok, NT, NTRoot]

      final case class Accept[Tok <: Token, NT <: NonTerminal, NTRoot <: NT](
          stackF: PartialFunction[List[StackElement[Tok, NT, NTRoot]], NTRoot],
      ) extends Action[Tok, NT, NTRoot]

    }

  }

  object buildTree {

    private[Grammar] def apply[Tok <: Token, NT <: NonTerminal, NTRoot <: NT](state0: State[Tok, NT, NTRoot])(source: Source, tokens: List[Tok]): Validated[NTRoot] =
      loop(source, state0, Nil, tokens)

    private def badGrammar(msg: String, span: Span): Marked[String] = Marked(s"Incorrectly designed grammar: $msg", span)

    @tailrec
    private def loop[Tok <: Token, NT <: NonTerminal, NTRoot <: NT](
        source: Source,
        currentState: State[Tok, NT, NTRoot],
        stack: List[StackElement[Tok, NT, NTRoot]],
        tokens: List[Tok],
    ): Validated[NTRoot] =
      currentState.onTerm.lift(tokens) match {
        case Some(action) =>
          action match {
            case Action.Shift(to) =>
              tokens match {
                case head :: tail => loop(source, to, StackElement(head.asLeft, currentState) :: stack, tail)
                case Nil          => badGrammar("Tried to shift without any remaining tokens", Span.EOF(source)).leftNel
              }
            case Action.Reduce(stackF) =>
              stackF.lift((currentState, stack)) match {
                case Some((tmpState, nt, newStack)) =>
                  // TODO (KR) : Not sure if this one is right...
                  tmpState.onNT.lift(nt) match {
                    case Some(to) => loop(source, to, StackElement(nt.asRight, tmpState) :: newStack, tokens)
                    case None =>
                      badGrammar(
                        s"Tried to shift on invalid non-terminal (s${currentState.id} -> s${tmpState.id}(${nt.ntName}) -> ???)",
                        tokens.headOption match {
                          case Some(tok) => tok.span
                          case None      => Span.EOF(source)
                        },
                      ).leftNel
                  }
                case None =>
                  badGrammar(
                    s"Tried to reduce on invalid stack (s${currentState.id})",
                    tokens.headOption match {
                      case Some(tok) => tok.span
                      case None      => Span.EOF(source)
                    },
                  ).leftNel
              }
            case Action.Accept(stackF) =>
              stackF.lift(stack) match {
                case Some(ntRoot) => ntRoot.asRight
                case None         => badGrammar(s"Tried to accept on invalid stack (s${currentState.id})", Span.EOF(source)).leftNel
              }
          }
        case None =>
          tokens.headOption match {
            case Some(tok) => Marked(s"Unexpected token (s${currentState.id}) : ${tok.tokName}", tok.span).leftNel
            case None      => Marked(s"Unexpected EOF (s${currentState.id})", Span.EOF(source)).leftNel
          }
      }

  }

}
