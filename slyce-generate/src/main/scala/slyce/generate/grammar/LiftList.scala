package slyce.generate.grammar

import scala.annotation.targetName

final case class LiftList[+A](
    before: List[A],
    lift: A,
    after: List[A],
) {

  def liftIdx: Int = before.size
  def toList: List[A] = before ::: lift :: after
  def size: Int = before.size + after.size + 1

  def map[B](f: A => B): LiftList[B] =
    LiftList(before.map(f), f(lift), after.map(f))

  @targetName("append")
  def :+[A2 >: A](a: A2): LiftList[A2] =
    LiftList(before, lift, after :+ a)

  override def toString: String = s"LiftList(${before.mkString(",")})($lift)(${after.mkString(",")})"

}
