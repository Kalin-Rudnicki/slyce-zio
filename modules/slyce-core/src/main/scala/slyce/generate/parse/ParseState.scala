package slyce.generate.parse

final case class ParseState(
    // FIX-PRE-MERGE (KR) :
)

// FIX-PRE-MERGE (KR) : remove
/*

  final case class ParseState(
      id: Int,
      actionsOnNonTerminals: Map[ExpandedGrammar.Identifier.NonTerminal, ParseState.Action.Push],
      lookAhead: ParseState.Action.LookAhead,
  )
  object ParseState {

    sealed trait Action
    object Action {
      sealed trait Simple extends Action
      sealed trait EOFAction extends Simple

      case object Accept extends EOFAction
      final case class Reduce(nt: ExpandedGrammar.Identifier.NonTerminal, prodNIdx: Int) extends EOFAction
      final case class Push(toStateId: Int) extends Simple
      final case class LookAhead( // TODO (KR) : I don't know if I like this name...
          actionsOnTerminals: Map[ExpandedGrammar.Identifier.Term, Action],
          actionOnEOF: Option[Action.EOFAction],
      ) extends Action
    }

  }

 */
