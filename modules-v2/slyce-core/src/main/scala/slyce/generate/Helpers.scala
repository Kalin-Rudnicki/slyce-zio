package slyce.generate

import scala.annotation.tailrec

private[slyce] object Helpers {

  @tailrec
  def findAll[T](
      unseen: Set[T],
      seen: Set[T] = Set.empty[T],
  )(
      findF: T => Set[T],
  ): Set[T] = {
    val newSeen = seen | unseen
    val newUnseen = unseen.flatMap(findF) &~ newSeen
    if newUnseen.nonEmpty then findAll(newUnseen, newSeen)(findF)
    else newSeen
  }

}
