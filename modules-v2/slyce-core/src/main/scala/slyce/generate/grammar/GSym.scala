package slyce.generate.grammar

/** Grammar symbol identity for expansion + LR table keys. */
sealed trait GSym {
  def label: String
  override def toString: String = label
}
object GSym {

  /** Product terminal. */
  final case class Term(label: String) extends GSym

  /** Named non-terminal (product or sum). */
  final case class Nt(label: String) extends GSym

  /** Anonymous list non-terminal phases after expansion. */
  final case class ListNt(id: String, phase: ListPhase) extends GSym {
    override def label: String = s"$$list[$id].$phase"
  }

  enum ListPhase {
    case Simple, Head, Tail
  }

  /** Anonymous optional non-terminal. */
  final case class OptNt(childLabel: String) extends GSym {
    override def label: String = s"$$opt[$childLabel]"
  }

}
