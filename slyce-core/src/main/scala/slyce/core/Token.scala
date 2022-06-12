package slyce.core

import klib.utils.*

trait Token extends Product {
  val tokName: String
  val text: String
  val span: Span.Highlight
  final val markedText: Marked[String] = Marked(text, span)
}
object Token {

  trait Const { self: Token =>
    final val text = tokName
    override def productPrefix: String = tokName
  }

}
