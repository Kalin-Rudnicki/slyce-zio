package slyce.parse

import oxygen.predef.core.*

import slyce.core.*

sealed trait ParseError extends Error {
  val source: Source
  def detail: String
  override def errorMessage: Text = detail.toText

  /** Point span for caret diagnostics when a position is known. */
  def toMarked: Marked[String] =
    this match {
      case e: ParseError.UnexpectedInput =>
        Marked(e.detail, Span.Range(e.source, e.at, e.at))
      case e: ParseError.LexerError =>
        Marked(e.detail, Span.Range(e.source, e.at, e.at))
      case e: ParseError.UnexpectedEOF =>
        val p = e.source.positions(e.source.length)
        Marked(e.detail, Span.Range(e.source, p, p))
      case e: ParseError.Internal =>
        val p = e.source.positions(e.source.length)
        Marked(e.detail, Span.Range(e.source, p, p))
    }

  /** Pretty multi-line diagnostic for this error on its source. */
  def markedMessage(config: Source.Config = Source.Config.Default): String =
    Source.mark(source, List(toMarked), config)
}
object ParseError {

  final case class UnexpectedInput(source: Source, at: Position, detail: String) extends ParseError

  final case class UnexpectedEOF(source: Source, detail: String) extends ParseError

  final case class LexerError(source: Source, at: Position, detail: String) extends ParseError

  final case class Internal(source: Source, detail: String) extends ParseError

  sealed trait Lexer extends ParseError
  sealed trait Grammar extends ParseError

}
