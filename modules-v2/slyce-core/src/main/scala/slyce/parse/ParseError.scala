package slyce.parse

import oxygen.predef.core.*

import slyce.core.*

sealed trait ParseError extends Error {
  val source: Source
}
object ParseError {

  sealed trait Lexer extends ParseError

  sealed trait Grammar extends ParseError

}
