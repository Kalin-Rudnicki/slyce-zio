package slyce.ignore.recursive

import oxygen.predef.core.*

import slyce.core.*
import slyce.parse.*

/** =====| Recursive-operator expressions + ignore slots (the phantom-look-ahead regression) |=====
  *
  * Before the prefix-tree look-ahead fix, a recursive operator-expression grammar with an optional ignore slot around the operator could NOT build an LR table at any finite `k`: the look-ahead
  * computation over-approximated FOLLOW over the recursive non-terminal and fabricated phantom (invalid) token runs, so the conflict path grew forever. These grammars are genuinely LR(2); they must
  * build at `k <= 3`.
  *
  * Terminals live in THIS file (the derivation macro requires a grammar's `@regex` terminals to be co-located with its `Parser.derived` call).
  */

// =====| Terminals |=====

/** A SINGLE maximal-munch ignore terminal: a whole run of whitespace / line-comments / block-comments is one token, so a slot only ever needs to be `Noise?` (0-or-1), i.e. bounded.
  */
@regex("([ \\t\\n\\r]+|//[^\\n]*\\n?|/\\*([^*]|\\*[^/])*\\*/)+".r)
final case class Noise(text: String, span: Span.Range) extends Terminal

@regex("\\(".r) final case class `(`(text: String, span: Span.Range) extends Terminal
@regex("\\)".r) final case class `)`(text: String, span: Span.Range) extends Terminal
@regex(":".r) final case class `:`(text: String, span: Span.Range) extends Terminal
@regex("[-+*/]".r) final case class Op(text: String, span: Span.Range) extends Terminal

@regex("[a-zA-Z_][a-zA-Z0-9_]*".r) final case class Ident(text: String, span: Span.Range) extends Terminal

@regex("[0-9]+".r) final case class Num(text: String, span: Span.Range, value: BigInt) extends Terminal
object Num {
  given BuildTerminal[Num] = BuildTerminal.attemptDecode1(BigInt(_))(Num.apply)
}

// =====| Recursive infix expression: Expr = Atom | Atom op Expr ; Atom = Num | Ident | ( Expr ) |=====

sealed trait Expr extends NonTerminal

/** Atom is a distinct sub-sum so `BinOp`'s lhs is an atom (keeping `Expr = Atom | Atom op Expr` unambiguous). */
sealed trait Atom extends Expr

/** Bare terminals are wrapped in nonterminal atoms (the derivation macro wants a sum's members uniform). */
final case class NumAtom(n: Num) extends Atom {
  override val span: Span.Range = n.span
}
final case class IdentAtom(id: Ident) extends Atom {
  override val span: Span.Range = id.span
}

/** `( Noise? Expr Noise? )` — the parenthesized re-entry that makes the phantom bite. */
@ignoreBetweenOne[Noise]
final case class Paren(open: `(`, inner: Expr, close: `)`) extends Atom {
  override val span: Span.Range = open.span <> close.span
}

/** `Atom Noise? Op Noise? Expr` — right-recursive infix with an OPTIONAL ignore slot around the operator. */
@ignoreBetweenOne[Noise]
final case class BinOp(lhs: Atom, op: Op, rhs: Expr) extends Expr {
  override val span: Span.Range = lhs.span <> rhs.span
}

/** Root: owns the OUTER ignore (before/after), so `Noise` sits in FOLLOW(Expr) — the exact condition that made the reduce/shift after an `Atom` ("complete Expr" vs "lhs of an operator") unresolvable
  * pre-fix.
  */
@ignoreBeforeOne[Noise] @ignoreAfterOne[Noise]
final case class Doc(expr: Expr) extends NonTerminal {
  override val span: Span.Range = expr.span
}
object Doc {
  val parser: Parser[Doc] = Parser.derived[Doc](2)

  /** Left-to-right leaf values (Num) of an expression, for order-independent assertions. */
  def nums(e: Expr): List[BigInt] =
    e match {
      case n: NumAtom   => n.n.value :: Nil
      case _: IdentAtom => Nil
      case p: Paren     => nums(p.inner)
      case b: BinOp     => nums(b.lhs) ::: nums(b.rhs)
    }

  /** Left-to-right operator + identifier tokens, for structural assertions. */
  def ops(e: Expr): List[String] =
    e match {
      case _: NumAtom   => Nil
      case i: IdentAtom => i.id.text :: Nil
      case p: Paren     => ops(p.inner)
      case b: BinOp     => ops(b.lhs) ::: (b.op.text :: ops(b.rhs))
    }
}

// =====| Soft-keyword fork: two products sharing a leading Ident, split past an ignore slot |=====

sealed trait Decl extends NonTerminal

/** `Ident Noise? Ident` — e.g. `import foo`. */
@ignoreBetweenOne[Noise]
final case class Import(kw: Ident, name: Ident) extends Decl {
  override val span: Span.Range = kw.span <> name.span
}

/** `Ident Noise? : Noise? Ident` — e.g. `x : Int`. */
@ignoreBetweenOne[Noise]
final case class Def(name: Ident, colon: `:`, tpe: Ident) extends Decl {
  override val span: Span.Range = name.span <> tpe.span
}

object Decl {
  val parser: Parser[Decl] = Parser.derived[Decl](2)
}
