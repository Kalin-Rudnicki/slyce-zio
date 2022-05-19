package slyce.generate.grammar

final case class LiftList[+A](
    before: List[A],
    lift: A,
    after: List[A],
)
