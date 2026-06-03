package slyce.generate.parse

final case class TmpActionState(
    // FIX-PRE-MERGE (KR) :
)

// FIX-PRE-MERGE (KR) : remove
/*

    // TODO (KR) : Rename this?
    private final case class TmpActionState(
        actionsOnNonTerminals: Map[ExpandedGrammar.Identifier.NonTerminal, TmpActionState.Action.Push],
        lookAhead: TmpActionState.Action.LookAhead,
    )
    private object TmpActionState {

      sealed trait Action
      object Action {
        sealed trait EOFAction extends Action

        case object Accept extends Action.EOFAction
        final case class Reduce(production: Closure.Production) extends Action.EOFAction
        final case class Push(to: Closure) extends Action
        final case class LookAhead(
            actionsOnTerminals: Map[ExpandedGrammar.Identifier.Term, Action],
            actionOnEOF: Option[Action.EOFAction],
        ) extends Action
      }

    }

 */
