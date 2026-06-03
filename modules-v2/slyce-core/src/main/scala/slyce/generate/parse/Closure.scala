package slyce.generate.parse

final case class Closure (
                           // FIX-PRE-MERGE (KR) : 
                         )

// FIX-PRE-MERGE (KR) : remove
/*

    private final case class Closure(entries: Set[Closure.Entry]) {
      lazy val (finishedEntries: Set[Closure.Entry.Finished], unfinishedEntries: Set[Closure.Entry.Waiting]) =
        entries.partitionMap {
          case e: Closure.Entry.Finished => e.asLeft
          case e: Closure.Entry.Waiting  => e.asRight
        }
    }
    private object Closure {

      final case class Production(
          reducesTo: ReducesTo.Production,
          producesIds: List[ExpandedGrammar.Identifier],
      )

      // TODO (KR) : I would like for this to be cleaned up a bit
      sealed trait Entry {
        val reducesTo: ReducesTo
        val seen: List[ExpandedGrammar.Identifier]
        val lookAhead: List[Follow]
        final lazy val waitingList: List[ExpandedGrammar.Identifier] =
          this match {
            case Entry.Waiting(_, _, waiting, _) => waiting.toList
            case _: Entry.Finished               => Nil
          }
      }
      object Entry {

        final case class Finished(
            reducesTo: ReducesTo,
            seen: List[ExpandedGrammar.Identifier],
            lookAhead: List[Follow],
        ) extends Entry

        final case class Waiting(
            reducesTo: ReducesTo,
            seen: List[ExpandedGrammar.Identifier],
            waiting: NonEmptyList[ExpandedGrammar.Identifier],
            lookAhead: List[Follow],
        ) extends Entry

        def apply(reducesTo: ReducesTo, seen: List[ExpandedGrammar.Identifier], waiting: List[ExpandedGrammar.Identifier], lookAhead: List[Follow]): Entry =
          waiting.toNel match {
            case Some(waiting) => Entry.Waiting(reducesTo, seen, waiting, lookAhead)
            case None          => Entry.Finished(reducesTo, seen, lookAhead)
          }

      }

    }

 */
