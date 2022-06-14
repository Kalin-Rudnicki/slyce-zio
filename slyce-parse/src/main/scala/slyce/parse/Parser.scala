package slyce.parse

import slyce.core.{NonTerminal as NT, *}

trait Parser {

  type Terminal <: Token
  type NonTerminal <: NT
  type NTRoot <: NonTerminal

  val lexer: Lexer[Terminal]
  val grammar: Grammar[Terminal, NonTerminal, NTRoot]

  final def parse(source: Source): Validated[NTRoot] =
    lexer.tokenize(source).flatMap(grammar.buildTree(source, _))

}
