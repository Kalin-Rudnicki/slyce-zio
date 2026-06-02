package slyce.parse

import oxygen.predef.test.*

import slyce.core.*
import slyce.core.builtIn.*

object ParserSpec extends OxygenSpecDefault {

  ///////  ///////////////////////////////////////////////////////////////

  @regex("\"".r) final case class `"`(text: String, span: Span.Range) extends Terminal
  @regex("\\(".r) final case class `(`(text: String, span: Span.Range) extends Terminal
  @regex("\\)".r) final case class `)`(text: String, span: Span.Range) extends Terminal
  @regex(":=".r) final case class `:=`(text: String, span: Span.Range) extends Terminal

  ///////  ///////////////////////////////////////////////////////////////

  sealed trait Literal extends Element { self: Terminal | NonTerminal => }

  @regex("-?\\d+".r)
  final case class IntLit(text: String, span: Span.Range, value: BigInt) extends Literal, Terminal
  object IntLit {
    given BuildTerminal[IntLit] = BuildTerminal.attemptDecode1(BigInt(_))(IntLit.apply)
  }

  @regex("-?\\d+\\.\\d+".r)
  final case class FloatLit(text: String, span: Span.Range, value: BigDecimal) extends Literal, Terminal
  object FloatLit {
    given BuildTerminal[FloatLit] = BuildTerminal.attemptDecode1(BigDecimal(_))(FloatLit.apply)
  }

  @regex("true|false".r)
  final case class BooleanLit(text: String, span: Span.Range, value: Boolean) extends Literal, Terminal
  object BooleanLit {
    given BuildTerminal[BooleanLit] = BuildTerminal.attemptDecode1(_.toBoolean)(BooleanLit.apply)
  }

  final case class StringLit(
      open: `"`,
      parts: ElementList[StringLit.Part],
      close: `"`,
  ) extends Literal,
        NonTerminal {
    override val span: Span.Range = open.span <> close.span
  }
  object StringLit {

    sealed trait Part extends Terminal

    @regex("[^\\n\"]+".r)
    final case class Chars(text: String, span: Span.Range) extends StringLit.Part

    @regex("\\\\.".r)
    final case class EscChar(text: String, span: Span.Range, char: Char) extends StringLit.Part
    object EscChar {

      private def char2(str: String): Either[String, Char] =
        if str.length == 2 && str(0) == '\\' then str(1).asRight
        else "Malformed".asLeft

      private def convert(c: Char): Either[String, Char] = c match
        case '\\' => '\\'.asRight
        case 'n'  => '\n'.asRight
        case 't'  => '\t'.asRight
        case '"'  => '"'.asRight
        case _    => "Invalid escape char".asLeft

      given BuildTerminal[EscChar] =
        (text, span) =>
          for {
            raw <- char2(text)
            converted <- convert(raw)
          } yield EscChar(text, span, converted)

    }

  }

  ///////  ///////////////////////////////////////////////////////////////

  sealed trait Ident extends Terminal

  @regex("[A-Za-z][A-Za-z_0-9]*|_[A-Za-z_0-9]+".r)
  final case class TextIdent(text: String, span: Span.Range) extends Ident

  sealed trait OpIdent extends Ident

  @regex("[+\\-]".r) // TODO (KR) : ... *
  final case class AddOp(text: String, span: Span.Range) extends OpIdent

  @regex("[*/%]".r) // TODO (KR) : ... *
  final case class MultOp(text: String, span: Span.Range) extends OpIdent

  ///////  ///////////////////////////////////////////////////////////////

  type Expr = Expr.Node2
  object Expr {

    sealed trait Leaf extends NonTerminal
    final case class LiteralExpr(lit: Literal) extends Expr.Leaf { override val span: Span.Range = lit.span }
    final case class IdentExpr(ident: Ident) extends Expr.Leaf { override val span: Span.Range = ident.span }
    final case class Wrap(open: `(`, wrapped: Expr, close: `)`) extends Leaf { override val span: Span.Range = open.span <> close.span }

    sealed trait Node1 extends NonTerminal
    object Node1 {
      final case class Next(leaf: Leaf) extends Node1 { override val span: Span.Range = leaf.span }
      final case class Bin(lhs: Leaf, op: AddOp, rhs: Node1) extends Node1 { override val span: Span.Range = lhs.span <> rhs.span }
    }

    sealed trait Node2 extends NonTerminal
    object Node2 {
      final case class Next(leaf: Node1) extends Node2 { override val span: Span.Range = leaf.span }
      final case class Bin(lhs: Node1, op: MultOp, rhs: Node2) extends Node2 { override val span: Span.Range = lhs.span <> rhs.span }
    }

  }

  ///////  ///////////////////////////////////////////////////////////////

  final case class Assign(
      ident: Ident,
      colon: `:=`,
      rhs: Expr,
  ) extends NonTerminal {
    override val span: Span.Range = ident.span <> rhs.span
  }

  final case class Program(
      assignments: ElementList[Assign],
      res: Expr,
  ) extends NonTerminal {
    override val span: Span.Range =
      assignments.headOption match
        case Some(assignment0) => assignment0.span <> res.span
        case None              => res.span
  }

  val parser: Parser[Program] = Parser.derived

  ///////  ///////////////////////////////////////////////////////////////

  override def testSpec: TestSpec =
    suite("ParserSpec")(
      // TODO (KR) :
    )

}
