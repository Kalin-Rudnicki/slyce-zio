package slyce.generate

import cats.syntax.either.*
import harness.core.*
import scala.annotation.tailrec

extension (chars: Set[Char]) {

  def groupChars: List[Either[Char, (Char, Char)]] = {
    @tailrec
    def loop(
        queue: List[Char],
        stack: List[Either[Char, (Char, Char)]],
    ): List[Either[Char, (Char, Char)]] =
      queue match {
        case lower :: tail =>
          @tailrec
          def loop2(
              queue: List[Char],
              current: Char,
          ): (Char, List[Char]) =
            queue match {
              case h :: t =>
                if (current + 1 == h)
                  loop2(
                    t,
                    h,
                  )
                else
                  (current, queue)
              case Nil =>
                (current, Nil)
            }

          val (upper, rest) = loop2(tail, lower)
          val entry =
            if (upper - lower > 0)
              (lower, upper).asRight
            else
              lower.asLeft
          loop(
            rest,
            entry :: stack,
          )
        case Nil =>
          stack.reverse
      }

    loop(chars.toList.sorted, Nil)
  }

  def prettyChars: String =
    prettyChars("Set")

  def prettyChars(name: String): String = {
    val list =
      chars.groupChars.map {
        case Left(c) =>
          c.unesc
        case Right((c1, c2)) =>
          s"${c1.unesc}-${c2.unesc}"
      }

    s"$name(${list.mkString(", ")})"
  }

}
