package slyce.generate.grammar

final case class NTGroup()

// FIX-PRE-MERGE (KR) : remove
/*

  enum NTGroup {
    case BasicNT(
        name: String,
        prods: NonEmptyList[List[Identifier]],
    )
    case LiftNT(
        name: String,
        prods: NonEmptyList[LiftList[Identifier]],
    )
    case ListNT(
        name: Either[String, AnonListNtId],
        listType: GrammarInput.NonTerminal.ListNonTerminal.Type,
        startProds: LiftList[Identifier],
        repeatProds: Option[LiftList[Identifier]],
    )
    case AssocNT(
        name: String,
        assocs: NonEmptyList[(Identifier, GrammarInput.NonTerminal.AssocNonTerminal.Type)],
        base: Either[
          NonEmptyList[List[Identifier]],
          NonEmptyList[LiftList[Identifier]],
        ],
    )
    case Optional(
        id: Identifier,
    )

    final lazy val rawNTs: NonEmptyList[RawNT] = convertNTGroup(this)

  }
 */
