package slyce.generate.grammar

final case class LiftList[+A](
    before: List[A],
    lift: A,
    after: List[A],
) {
  def liftIdx: Int = before.size
  def toList: List[A] = before ::: lift :: after
  def size: Int = before.size + after.size + 1
}
