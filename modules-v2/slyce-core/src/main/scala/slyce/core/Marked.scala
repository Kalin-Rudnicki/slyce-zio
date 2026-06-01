package slyce.core

final case class Marked[+S <: Span, +A](value: A, span: S)
object Marked {
  type Span[+A] = Marked[slyce.core.Span, A]
  type HasSource[+A] = Marked[slyce.core.Span.HasSource, A]
  type Range[+A] = Marked[slyce.core.Span.Range, A]
}
