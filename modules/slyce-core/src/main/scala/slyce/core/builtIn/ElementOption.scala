package slyce.core.builtIn

import slyce.core.*

sealed trait ElementOption[+Elem <: Element] extends NonTerminal {
  def toOption: Option[Elem]
}
object ElementOption {

  final case class Some[+Elem <: Element](value: Elem) extends ElementOption[Elem] {
    override val span: Span.Range = value.span
    override def toOption: Option[Elem] = scala.Some(value)
  }

  final case class None(span: Span.Range) extends ElementOption[Nothing] {
    override def toOption: Option[Nothing] = scala.None
  }

}
