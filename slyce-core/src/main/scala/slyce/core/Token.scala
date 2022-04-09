package slyce.core

trait Token {
  val tokName: String
  val text: String
  val span: Span
  final val markedText: Marked[String] = Marked(text, span)
}
