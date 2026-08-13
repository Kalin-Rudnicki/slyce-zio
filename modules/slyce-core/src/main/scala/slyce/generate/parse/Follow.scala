package slyce.generate.parse

final case class Follow(
    // FIX-PRE-MERGE (KR) :
)

// FIX-PRE-MERGE (KR) : remove
/*

    private final case class Follow(
        validTerminals: Set[ExpandedGrammar.Identifier.Term],
        eofIsValid: Boolean,
    ) {
      override def toString: String =
        (validTerminals.map(_.toString).toList.sorted ::: Option.when(eofIsValid)("$".toText.cyanFg.toString).toList)
          .mkString("Follow< ", ", ", " >")
          .toText
          .magentaBg
          .toString
    }

 */
