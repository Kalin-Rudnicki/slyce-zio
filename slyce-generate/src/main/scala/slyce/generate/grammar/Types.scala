package slyce.generate.grammar

import cats.data.NonEmptyList
import scala.annotation.tailrec

import slyce.core.*

sealed trait Element
object Element {
  sealed trait NonOptional extends Element
  final case class Optional(child: Element.NonOptional) extends Element
}

sealed trait Identifier extends Element.NonOptional
object Identifier {
  final case class Terminal(name: String) extends Identifier
  final case class NonTerminal(name: String) extends Identifier
  final case class Raw(text: String) extends Identifier

  def apply(str: String): Identifier = {
    @tailrec
    def identify(chars: List[Char]): Identifier =
      chars match {
        case Nil => Raw(str)
        case c :: rest =>
          if (c == '_') identify(rest)
          else if (c.isUpper) NonTerminal(str)
          else if (c.isLower) Terminal(str)
          else Raw(str)
      }

    identify(str.toList)
  }

}

sealed trait NonTerminal
object NonTerminal {

  enum StandardNonTerminal extends NonTerminal {
    case `:`(reductions: NonEmptyList[List[Marked[Element]]])
    case ^(reductions: NonEmptyList[LiftList[Marked[Element]]])
  }

  final case class ListNonTerminal(
      `type`: ListNonTerminal.Type,
      start: LiftList[Marked[Element]],
      repeat: Option[LiftList[Marked[Element]]],
  ) extends NonTerminal
      with Element.NonOptional
  object ListNonTerminal {
    enum Type { case *, + }
  }

  final case class AssocNonTerminal(
      assocElements: NonEmptyList[(Marked[AssocNonTerminal.Type], Marked[Element])],
      base: StandardNonTerminal,
  ) extends NonTerminal
  object AssocNonTerminal {
    enum Type { case Left, Right }
  }

}
