package slyce.core

type Marked[+A] = PolyMarked.Range[A]
object Marked {
  def apply[A](value: A, span: Span.Range): Marked[A] = PolyMarked(value, span)
}

final case class PolyMarked[+S <: Span, +A](value: A, span: S)
object PolyMarked {
  type Span[+A] = PolyMarked[slyce.core.Span, A]
  type HasSource[+A] = PolyMarked[slyce.core.Span.HasSource, A]
  type Range[+A] = PolyMarked[slyce.core.Span.Range, A]
}
