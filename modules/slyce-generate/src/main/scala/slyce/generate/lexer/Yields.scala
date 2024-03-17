package slyce.generate.lexer

import cats.syntax.option.*

import slyce.core.*

final case class Yields[I](
    yields: List[Marked[Yields.Yield]],
    toMode: Marked[Yields.ToMode[I]],
) {

  def yieldsTerminals: Set[String] =
    yields.flatMap {
      _.value match {
        case Yields.Yield.Terminal(name, _, _) => name.some
        case _                                 => None
      }
    }.toSet

  def map[I2](f: I => I2): Yields[I2] =
    Yields(
      yields,
      toMode.map(_.map(f)),
    )

}

object Yields {

  // TODO (KR) : Remove default arguments
  sealed trait Yield {
    val subString: (Option[Int], Option[Int])
  }
  object Yield {
    final case class Text(
        subString: (Option[Int], Option[Int]) = (None, None),
    ) extends Yield
    final case class Terminal(
        name: String,
        text: Option[String] = None,
        subString: (Option[Int], Option[Int]) = (None, None),
    ) extends Yield
    final case class ConstText(
        text: String,
        subString: (Option[Int], Option[Int]) = (None, None),
    ) extends Yield
  }

  sealed trait ToMode[+I] {

    final def map[I2](f: I => I2): ToMode[I2] =
      this match {
        case ToMode.Same       => ToMode.Same
        case ToMode.To(mode)   => ToMode.To(f(mode))
        case ToMode.Push(mode) => ToMode.Push(f(mode))
        case ToMode.Pop        => ToMode.Pop
      }

  }
  object ToMode {
    case object Same extends ToMode[Nothing]
    final case class To[I](mode: I) extends ToMode[I]
    final case class Push[I](mode: I) extends ToMode[I]
    // TODO (KR) : Eventual improvement will be allowing for arbitrary number of pops
    //           : eg. Pop(2), Pop(3), Pop(5)
    case object Pop extends ToMode[Nothing]
  }

}
