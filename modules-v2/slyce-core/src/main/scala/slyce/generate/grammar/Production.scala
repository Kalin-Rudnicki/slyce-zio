package slyce.generate.grammar

final case class Production(elements: List[GSym])
object Production {
  def apply(elements: GSym*): Production = Production(elements.toList)
}
