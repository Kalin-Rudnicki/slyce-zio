package slyce.parse

import oxygen.predef.core.*

import slyce.core.*

sealed trait ParseError extends Error {
  val source: Source
  def detail: String
  override def errorMessage: Text = detail.toText
}
object ParseError {

  final case class UnexpectedInput(source: Source, at: Position, detail: String) extends ParseError

  final case class UnexpectedEOF(source: Source, detail: String) extends ParseError

  final case class LexerError(source: Source, at: Position, detail: String) extends ParseError

  final case class Internal(source: Source, detail: String) extends ParseError

  sealed trait Lexer extends ParseError
  sealed trait Grammar extends ParseError

}
