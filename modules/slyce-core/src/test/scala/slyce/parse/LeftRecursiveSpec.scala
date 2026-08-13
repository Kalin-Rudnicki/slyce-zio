package slyce.parse

import oxygen.predef.test.*

import slyce.TestUtils.*
import slyce.core.*
import slyce.core.builtIn.*

/** Left-recursive products on the same sealed NT are valid LALR surface shapes (e.g. `Select(lhs: Expr, …) extends Expr`). Extraction/FIRST must not SO.
  */
object LeftRecursiveSpec extends OxygenSpecDefault {

  @regex("\\.".r)
  final case class Dot(text: String, span: Span.Range) extends Terminal

  @regex("[a-z]+".r)
  final case class Ident(text: String, span: Span.Range) extends Terminal

  sealed trait TypeExpr extends NonTerminal

  final case class TypeName(name: Ident) extends TypeExpr {
    override val span: Span.Range = name.span
  }

  final case class TypeSelect(lhs: TypeExpr, dot: Dot, name: Ident) extends TypeExpr {
    override val span: Span.Range = lhs.span <> name.span
  }

  object TypeExpr {
    val parser: Parser[TypeExpr] = Parser.derived[TypeExpr](2)
  }

  override def testSpec: TestSpec =
    suite("LeftRecursiveSpec")(
      parsesTo(TypeExpr.parser, "a") { s =>
        TypeName(Ident("a", spanOf(s, "a")))
      },
      parsesTo(TypeExpr.parser, "a.b") { s =>
        TypeSelect(
          TypeName(Ident("a", span(s, 0, 1))),
          Dot(".", span(s, 1, 2)),
          Ident("b", span(s, 2, 3)),
        )
      },
      parsesTo(TypeExpr.parser, "a.b.c") { s =>
        TypeSelect(
          TypeSelect(
            TypeName(Ident("a", span(s, 0, 1))),
            Dot(".", span(s, 1, 2)),
            Ident("b", span(s, 2, 3)),
          ),
          Dot(".", span(s, 3, 4)),
          Ident("c", span(s, 4, 5)),
        )
      },
      failsToParse(TypeExpr.parser, ""),
      failsToParse(TypeExpr.parser, "."),
      failsToParse(TypeExpr.parser, "a."),
    )

}
