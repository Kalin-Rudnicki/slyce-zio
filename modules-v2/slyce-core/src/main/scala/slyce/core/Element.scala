package slyce.core

trait Element { self: Terminal | NonTerminal =>
  val span: Span.Range
}

trait Terminal extends Element {
  val text: String
  final def markedText: Marked.Range[String] = Marked(text, span)
}

trait NonTerminal extends Element
