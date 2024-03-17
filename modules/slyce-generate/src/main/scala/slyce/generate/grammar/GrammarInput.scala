package slyce.generate.grammar

import cats.data.NonEmptyList
import scala.annotation.tailrec

import slyce.core.*

final case class GrammarInput(
    startNT: Marked[String],
    nonTerminals: List[GrammarInput.NamedNonTerminal],
    maxLookAhead: Marked[Int],
)
object GrammarInput {

  final case class NamedNonTerminal(
      name: Marked[Identifier.NonTerminal],
      nonTerminal: NonTerminal,
  )

  sealed trait Element {

    def toNonOpt: (Boolean, Element.NonOptional) =
      this match {
        case element: Element.NonOptional => (false, element)
        case Element.Optional(element)    => (true, element)
      }

  }
  object Element {
    sealed trait NonOptional extends Element
    final case class Optional(child: Element.NonOptional) extends Element
  }

  enum Identifier extends Element.NonOptional {
    case Terminal(name: String)
    case NonTerminal(name: String)
    case Raw(text: String)
  }
  object Identifier {

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
      case `:`(productions: NonEmptyList[List[Marked[Element]]])
      case ^(productions: NonEmptyList[LiftList[Marked[Element]]])
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

}
