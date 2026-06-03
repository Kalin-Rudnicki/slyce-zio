package slyce.generate.grammar

final case class Expansion(
    // FIX-PRE-MERGE (KR) :
)

// FIX-PRE-MERGE (KR) : remove
/*

    private final case class Expansion[+A](
        value: A,
        ntGroups: List[NTGroup],
    ) {
      def map[B](f: A => B): Expansion[B] = Expansion(f(value), ntGroups)
    }
    private object Expansion {

      def mergeNTGroup(ntGroup: NTGroup)(includes: List[Expansion[?]]*): Expansion[Identifier] =
        Expansion(
          ntGroupHead(ntGroup),
          ntGroup :: includes.toList.flatten.flatMap(_.ntGroups),
        )

    }

 */
