package slyce.core

trait Element { self: Terminal | NonTerminal => // this mechanism is used to prevent wonky inheritance patterns
  val slyceElementType: String // this mechanism is used to prevent wonky inheritance patterns
  def span: Span.Range
}

trait Terminal extends Element {
  override final val slyceElementType: String = "Terminal"
  val text: String
  override val span: Span.Range
  final def markedText: Marked[String] = Marked(text, span)
}

trait NonTerminal extends Element {
  override final val slyceElementType: String = "NonTerminal"

  override val span: Span.Range
  // TODO (KR) : something like:
  //           : private var _span: Span.Range
  //           : override final def span: Span.Range = if _span != null then _span else ...

}
