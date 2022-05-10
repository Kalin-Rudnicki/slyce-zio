package slyce.generate.debugging

import cats.syntax.either.*

import slyce.core.*
import slyce.generate.*
import slyce.generate.lexer.*

final case class Result(
    lexer: LexerInput,
    nfa: Validated[NFA],
    dfa: Validated[DFA],
)
object Result {

  def build(
      lexer: LexerInput,
  ): Result = {
    val (nfa, dfa) =
      NFA.fromLexer(lexer) match {
        case right @ Right(nfa) => (right, DFA.fromNFA(nfa))
        case left @ Left(_)     => (left, Marked("Unable to attempt building DFA", Span.Unknown).leftNel)
      }

    Result(
      lexer = lexer,
      nfa = nfa,
      dfa = dfa,
    )
  }

}
