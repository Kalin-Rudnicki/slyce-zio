package slyce.core

import harness.core.*

trait Token extends Product {
  val tokName: String
  val text: String
  val span: Span.Highlight
  final val markedText: Marked[String] = Marked(text, span)
}
object Token {

  def mark(t: Token): Marked[String] =
    Marked(if (t.isInstanceOf[Token.Const]) t.tokName else s"${t.tokName} : ${t.text.unesc}", t.span)

  trait Const { self: Token =>
    final val text = tokName
    override def productPrefix: String = tokName
  }

}
