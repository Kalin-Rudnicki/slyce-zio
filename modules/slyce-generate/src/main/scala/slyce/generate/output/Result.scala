package slyce.generate.output

import cats.syntax.parallel.*

import slyce.core.*
import slyce.generate.*
import slyce.generate.grammar.*
import slyce.generate.lexer.*

final case class Result private (
    lexer: LexerInput,
    nfa: NFA,
    dfa: DFA,
    grammar: GrammarInput,
    expandedGrammar: ExpandedGrammar,
    parsingTable: ParsingTable,
    extras: Extras,
)
object Result {

  def build(lexer: LexerInput, grammar: GrammarInput): Validated[Result] = {
    val lexerPart: Validated[(LexerInput, NFA, DFA)] =
      for {
        nfa <- NFA.fromLexer(lexer)
        dfa <- DFA.fromNFA(nfa)
      } yield (lexer, nfa, dfa)
    val grammarPart: Validated[(GrammarInput, ExpandedGrammar, ParsingTable)] = {
      val expandedGrammar = ExpandedGrammar.fromGrammar(grammar)
      ParsingTable.fromExpandedGrammar(expandedGrammar).map((grammar, expandedGrammar, _))
    }

    (lexerPart, grammarPart).parTupled.flatMap { case ((lexer, nfa, dfa), (grammar, expandedGrammar, parsingTable)) =>
      Extras.build(dfa, expandedGrammar).map {
        Result(
          lexer,
          nfa,
          dfa,
          grammar,
          expandedGrammar,
          parsingTable,
          _,
        )
      }
    }
  }

}
