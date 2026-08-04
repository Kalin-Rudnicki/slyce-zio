package slyce.parse

import oxygen.predef.core.*

import slyce.core.*

sealed trait ParseError extends Error {
  val source: Source
  def message: String
  override def getMessage: String = message
}
object ParseError {

  final case class UnexpectedInput(source: Source, at: Position, detail: String) extends ParseError {
    override def message: String = s"Unexpected input at ${at.show}: $detail"
  }

  final case class UnexpectedEOF(source: Source, detail: String) extends ParseError {
    override def message: String = s"Unexpected EOF: $detail"
  }

  final case class LexerError(source: Source, at: Position, detail: String) extends ParseError {
    override def message: String = s"Lexer error at ${at.show}: $detail"
  }

  final case class Internal(source: Source, detail: String) extends ParseError {
    override def message: String = s"Internal parser error: $detail"
  }

  sealed trait Lexer extends ParseError
  sealed trait Grammar extends ParseError

}
