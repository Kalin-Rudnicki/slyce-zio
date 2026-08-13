package slyce.core.builtIn

import oxygen.predef.core.*

import slyce.core.*

// [  ]
// [ Before, A, After ]
// [ Before, A, Between, A, After ]
// [ Before, A, Between, A, Between, A, After ]

sealed trait VElementList[+Elem <: Element, +Before <: Element, +Between <: Element, +After <: Element] extends NonTerminal {
  def toList: List[Elem]
  def toVList: Option[(Before, Elem, List[(Between, Elem)], After)]
}

final case class NonEmptyVElementList[+Elem <: Element, +Before <: Element, +Between <: Element, +After <: Element](
    before: Before,
    head: Elem,
    tail: NonEmptyVElementListTail[Elem, Between, After],
) extends VElementList[Elem, Before, Between, After] {
  override val span: Span.Range = head.span <> tail.span
  def toNonEmptyList: NonEmptyList[Elem] = NonEmptyList(head, tail.toList)
  override def toList: List[Elem] = head :: tail.toList
  def toNonEmptyVList: (Before, Elem, List[(Between, Elem)], After) = {
    val (list, after) = tail.toVList
    (before, head, list, after)
  }
  override def toVList: Option[(Before, Elem, List[(Between, Elem)], After)] = toNonEmptyVList.some
}

sealed trait VElementListTail[+Elem <: Element, +Between <: Element, +After <: Element] extends NonTerminal {
  def toList: List[Elem]
  def toVList: (List[(Between, Elem)], After)
}

final case class NonEmptyVElementListTail[+Elem <: Element, +Between <: Element, +After <: Element](
    between: Between,
    head: Elem,
    tail: VElementListTail[Elem, Between, After],
) extends VElementListTail[Elem, Between, After] {
  override val span: Span.Range = between.span <> tail.span
  override def toList: List[Elem] = head :: tail.toList
  override def toVList: (List[(Between, Elem)], After) = {
    val (list, after) = tail.toVList
    ((between, head) :: list, after)
  }
}

final case class VElementListTailNil[+After <: Element](
    after: After,
) extends VElementListTail[Nothing, Nothing, After] {
  override val span: Span.Range = after.span
  override def toList: List[Nothing] = Nil
  override def toVList: (List[(Nothing, Nothing)], After) = (Nil, after)
}

final case class EmptyVElementList(span: Span.Range) extends VElementList[Nothing, Nothing, Nothing, Nothing] {
  override def toList: List[Nothing] = Nil
  override def toVList: Option[(Nothing, Nothing, List[(Nothing, Nothing)], Nothing)] = None
}
