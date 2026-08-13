package slyce.calculator.model

import slyce.core.*
import slyce.core.builtIn.*
import slyce.parse.*

// =====| Punctuation / ops |=====

@regex("=".r) final case class `=`(text: String, span: Span.Range) extends Terminal
@regex("\\(".r) final case class `(`(text: String, span: Span.Range) extends Terminal
@regex("\\)".r) final case class `)`(text: String, span: Span.Range) extends Terminal
@regex(";".r) final case class `;`(text: String, span: Span.Range) extends Terminal

/** Surface `_` (cannot be named `` `_` `` — Scala treats that as the wildcard). */
@regex("_".r) final case class Underscore(text: String, span: Span.Range) extends Terminal

@regex("[+\\-]".r)
final case class AddOp(text: String, span: Span.Range) extends Terminal

@regex("[*/]".r)
final case class MulOp(text: String, span: Span.Range) extends Terminal

// =====| Atoms |=====

/** Not bare `_` (that is [[Underscore]] for [[GenericBoxEmpty]]); `_foo` still ok. */
@regex("(?:[A-Za-z][A-Za-z_0-9]*|_[A-Za-z_0-9]+)".r)
final case class Ident(text: String, span: Span.Range) extends Terminal

@regex("-?\\d+".r)
final case class IntLit(text: String, span: Span.Range, value: BigInt) extends Terminal
object IntLit {
  given BuildTerminal[IntLit] = BuildTerminal.attemptDecode1(BigInt(_))(IntLit.apply)
}

// =====| Program = List[Assignment] |=====

/** Simple calculator program: zero or more assignments.
  *
  * x = 1 + 2; y = x * 3;
  */
final case class Program(
    assignments: ElementList[Assignment],
) extends NonTerminal {
  override val span: Span.Range = assignments match {
    case n: NonEmptyElementList[?] => n.span
    case n: ElementNil             => n.span
  }
}
object Program {

  val parser: Parser[Program] = Parser.derived[Program](1)

}

/** RHS is [[GenericBox]]`[Expr]`: either a real expression (`Has`) or empty (`Empty` = `_`, so surface form is `name = _;`). Nested generic sum — exercised via [[Program.parser]], not only as a
  * standalone root.
  */
final case class Assignment(
    name: Ident,
    eq: `=`,
    expr: GenericBox[Expr],
    semi: `;`,
) extends NonTerminal {
  override val span: Span.Range = name.span <> semi.span
}

// =====| Expression (Mul binds tighter than Add) |=====

type Expr = Expr.Add
object Expr {

  sealed trait Add extends NonTerminal
  object Add {
    final case class Next(mul: Mul) extends Add {
      override val span: Span.Range = mul.span
    }
    final case class Bin(lhs: Mul, op: AddOp, rhs: Add) extends Add {
      override val span: Span.Range = lhs.span <> rhs.span
    }
  }

  sealed trait Mul extends NonTerminal
  object Mul {
    final case class Next(atom: Atom) extends Mul {
      override val span: Span.Range = atom.span
    }
    final case class Bin(lhs: Atom, op: MulOp, rhs: Mul) extends Mul {
      override val span: Span.Range = lhs.span <> rhs.span
    }
  }

  sealed trait Atom extends NonTerminal
  final case class Lit(value: IntLit) extends Atom {
    override val span: Span.Range = value.span
  }
  final case class Ref(name: Ident) extends Atom {
    override val span: Span.Range = name.span
  }
  final case class Paren(open: `(`, expr: Add, close: `)`) extends Atom {
    override val span: Span.Range = open.span <> close.span
  }

}

// =====| Generic sum (SumGeneric type params; used in Assignment + standalone roots) |=====

/** Generic sum — `Has[A]` forwards raw type param `A`; `Empty` fixes `A = Nothing`.
  *
  * Used inside the calculator AST as [[Assignment.expr]] (`GenericBox[Expr]`), and also as monomorphized standalone roots (`GenericBox[Ident]` / `GenericBox[IntLit]`).
  */
sealed trait GenericBox[+A <: Element] extends NonTerminal
final case class GenericBoxHas[A <: Element](value: A) extends GenericBox[A] {
  override val span: Span.Range = value.span
}
final case class GenericBoxEmpty(hole: Underscore) extends GenericBox[Nothing] {
  override val span: Span.Range = hole.span
}
object GenericBox {

  type Has[A <: Element] = GenericBoxHas[A]
  val Has = GenericBoxHas
  type Empty = GenericBoxEmpty
  val Empty = GenericBoxEmpty

  /** Standalone monomorphizations (terminals as `A`) — also covered nested via [[Program.parser]]. */
  val parserIdent: Parser[GenericBox[Ident]] = Parser.derived[GenericBox[Ident]](1)
  val parserIntLit: Parser[GenericBox[IntLit]] = Parser.derived[GenericBox[IntLit]](1)

}
