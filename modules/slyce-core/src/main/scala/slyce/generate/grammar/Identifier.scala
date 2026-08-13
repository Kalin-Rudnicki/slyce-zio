package slyce.generate.grammar

import slyce.generate.*

// FIX-PRE-MERGE (KR) :
private[slyce] sealed trait Identifier {
  val underlyingType: ExtractedType
}

private[slyce] sealed trait NonTerminalIdentifier extends Identifier

private[slyce] sealed trait TerminalIdentifier extends Identifier

// FIX-PRE-MERGE (KR) : remove
/*

  sealed trait Identifier
  object Identifier {

    enum NonTerminal extends Identifier {

      case NamedNt(name: String)
      case NamedListNtTail(name: String)
      case AnonListNt(key: AnonListNtId, `type`: NonTerminal.ListType)
      case AssocNt(name: String, idx: Int)
      case AnonOptNt(identifier: Identifier)
    }
    object NonTerminal {
      enum ListType { case Simple, Head, Tail }
    }

    sealed trait Term extends Identifier
    object Term {
      final case class Terminal(name: String) extends Term
      final case class Raw(name: String) extends Term {
        override def toString: String = s"Raw(${name.unesc})"
      }
    }

  }

 */
