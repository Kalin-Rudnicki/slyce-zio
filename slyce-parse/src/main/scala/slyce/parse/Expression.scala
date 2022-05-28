package slyce.parse

sealed trait Expression[+Operand, +Operator] {

  def mapOperand[Operand2](f: Operand => Operand2): Expression[Operand2, Operator] =
    this match {
      case Expression.Node(left, op, right) => Expression.Node(left.mapOperand(f), op, right.mapOperand(f))
      case Expression.Leaf(operand)         => Expression.Leaf(f(operand))
    }

  def mapOperator[Operator2](f: Operator => Operator2): Expression[Operand, Operator2] =
    this match {
      case Expression.Node(left, op, right) => Expression.Node(left.mapOperator(f), f(op), right.mapOperator(f))
      case leaf @ Expression.Leaf(_)        => leaf
    }

  def mapBoth[Operand2, Operator2](operandF: Operand => Operand2, operatorF: Operator => Operator2): Expression[Operand2, Operator2] =
    this match {
      case Expression.Node(left, op, right) =>
        Expression.Node(
          left.mapBoth(operandF, operatorF),
          operatorF(op),
          right.mapBoth(operandF, operatorF),
        )
      case Expression.Leaf(operand) =>
        Expression.Leaf(operandF(operand))
    }

}
object Expression {

  final case class Leaf[+Operand](operand: Operand) extends Expression[Operand, Nothing]
  final case class Node[+Operand, +Operator](
      left: Expression[Operand, Operator],
      op: Operator,
      right: Expression[Operand, Operator],
  ) extends Expression[Operand, Operator]

  def apply[Operand](operand: Operand): Expression[Operand, Nothing] =
    Leaf(operand)

  def apply[Operand, Operator](
      left: Expression[Operand, Operator],
      op: Operator,
      right: Expression[Operand, Operator],
  ): Expression[Operand, Operator] =
    Node(left, op, right)

}
