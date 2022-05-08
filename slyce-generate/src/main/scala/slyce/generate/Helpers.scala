package slyce.generate

import scala.annotation.tailrec

object Helpers {

  @tailrec
  def findAll[T](
      unseen: Set[T],
      seen: Set[T] = Set.empty[T],
  )(
      findF: T => Set[T],
  ): Set[T] = {
    val newSeen = seen | unseen
    val newUnseen = unseen.flatMap(findF) &~ newSeen

    if (newUnseen.nonEmpty)
      findAll(newUnseen, newSeen)(findF)
    else
      newSeen
  }

}

extension [A](a: A) {

  def someWhen(f: A => Boolean): Option[A] =
    Option.when(f(a))(a)

}
