package slyce.core.builtIn

import oxygen.predef.core.*

import slyce.core.*

sealed trait ElementList[+Elem <: Element] extends NonTerminal {
  def toList: List[Elem]
}

final case class NonEmptyElementList[+Elem <: Element](head: Elem, tail: ElementList[Elem]) extends ElementList[Elem] {
  override val span: Span.Range = head.span <> tail.span
  def toNonEmptyList: NonEmptyList[Elem] = NonEmptyList(head, tail.toList)
  override def toList: List[Elem] = head :: tail.toList
}

final case class ElementNil(span: Span.Range) extends ElementList[Nothing] {
  override def toList: List[Nothing] = Nil
}
