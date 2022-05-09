package slyce.core

import klib.utils.*

final case class Marked[+T](
    value: T,
    span: Span,
) {

  inline def as[T2](f: => T2): Marked[T2] =
    map(_ => f)

  def map[T2](f: T => T2): Marked[T2] =
    Marked(f(value), span)

  def flatMap[T2](f: T => Marked[T2]): Marked[T2] = {
    val marked2 = f(value)
    Marked(marked2.value, Span.joinSpans(span, marked2.span))
  }

  def toString(showAbsolute: Boolean): String = {
    val str: String =
      value.asInstanceOf[Matchable] match {
        case str: String => str.unesc
        case any         => any.toString
      }

    s"$str @ ${span.toString(showAbsolute)}"
  }

  override def toString: String =
    toString(false)

}
