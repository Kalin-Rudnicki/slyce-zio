package slyce.calculator

import oxygen.predef.test.*

import slyce.TestUtils.*
import slyce.calculator.model.*
import slyce.calculator.model.Expr
import slyce.core.*
import slyce.core.builtIn.*

object CalculatorSpec extends OxygenSpecDefault {

  private def lit(s: Source, text: String, from: Int = 0): Expr.Atom =
    Expr.Lit(IntLit(text, spanOf(s, text, from), BigInt(text)))

  private def ref(s: Source, name: String, from: Int = 0): Expr.Atom =
    Expr.Ref(Ident(name, spanOf(s, name, from)))

  private def mulNext(atom: Expr.Atom): Expr.Mul = Expr.Mul.Next(atom)
  private def addNext(mul: Expr.Mul): Expr.Add = Expr.Add.Next(mul)
  private def atomExpr(atom: Expr.Atom): Expr = addNext(mulNext(atom))

  private def boxed(expr: Expr): GenericBox[Expr] = GenericBox.Has(expr)

  private def assign(s: Source, name: String, expr: Expr, semiAt: Int): Assignment =
    Assignment(
      Ident(name, spanOf(s, name)),
      `=`("=", spanOf(s, "=")),
      boxed(expr),
      `;`(";", span(s, semiAt, semiAt + 1)),
    )

  override def testSpec: TestSpec =
    suite("CalculatorSpec")(
      suite("valid")(
        parsesTo(Program.parser, "") { s =>
          Program(elementList[Assignment](eofSpan(s))())
        },
        parsesTo(Program.parser, "x=1;") { s =>
          Program(
            elementList[Assignment](eofSpan(s))(
              assign(s, "x", atomExpr(lit(s, "1")), semiAt = 3),
            ),
          )
        },
        parsesTo(Program.parser, "x=1;y=2;") { s =>
          Program(
            elementList[Assignment](eofSpan(s))(
              assign(s, "x", atomExpr(lit(s, "1")), semiAt = 3),
              Assignment(
                Ident("y", spanOf(s, "y")),
                `=`("=", spanOf(s, "=", from = 4)),
                boxed(atomExpr(lit(s, "2"))),
                `;`(";", span(s, 7, 8)),
              ),
            ),
          )
        },
        parsesTo(Program.parser, "x=1+2;") { s =>
          Program(
            elementList[Assignment](eofSpan(s))(
              assign(
                s,
                "x",
                Expr.Add.Bin(
                  mulNext(lit(s, "1")),
                  AddOp("+", spanOf(s, "+")),
                  addNext(mulNext(lit(s, "2"))),
                ),
                semiAt = 5,
              ),
            ),
          )
        },
        parsesTo(Program.parser, "x=1+2+3;") { s =>
          Program(
            elementList[Assignment](eofSpan(s))(
              assign(
                s,
                "x",
                Expr.Add.Bin(
                  mulNext(lit(s, "1")),
                  AddOp("+", spanOf(s, "+")),
                  Expr.Add.Bin(
                    mulNext(lit(s, "2")),
                    AddOp("+", spanOf(s, "+", from = 4)),
                    addNext(mulNext(lit(s, "3"))),
                  ),
                ),
                semiAt = 7,
              ),
            ),
          )
        },
        parsesTo(Program.parser, "x=2*3;") { s =>
          Program(
            elementList[Assignment](eofSpan(s))(
              assign(
                s,
                "x",
                addNext(
                  Expr.Mul.Bin(
                    lit(s, "2"),
                    MulOp("*", spanOf(s, "*")),
                    mulNext(lit(s, "3")),
                  ),
                ),
                semiAt = 5,
              ),
            ),
          )
        },
        parsesTo(Program.parser, "x=1+2*3;") { s =>
          // 1 + (2 * 3)
          Program(
            elementList[Assignment](eofSpan(s))(
              assign(
                s,
                "x",
                Expr.Add.Bin(
                  mulNext(lit(s, "1")),
                  AddOp("+", spanOf(s, "+")),
                  addNext(
                    Expr.Mul.Bin(
                      lit(s, "2"),
                      MulOp("*", spanOf(s, "*")),
                      mulNext(lit(s, "3")),
                    ),
                  ),
                ),
                semiAt = 7,
              ),
            ),
          )
        },
        parsesTo(Program.parser, "x=(1+2)*3;") { s =>
          Program(
            elementList[Assignment](eofSpan(s))(
              assign(
                s,
                "x",
                addNext(
                  Expr.Mul.Bin(
                    Expr.Paren(
                      `(`("(", spanOf(s, "(")),
                      Expr.Add.Bin(
                        mulNext(lit(s, "1")),
                        AddOp("+", spanOf(s, "+")),
                        addNext(mulNext(lit(s, "2"))),
                      ),
                      `)`(")", spanOf(s, ")")),
                    ),
                    MulOp("*", spanOf(s, "*")),
                    mulNext(lit(s, "3")),
                  ),
                ),
                semiAt = 9,
              ),
            ),
          )
        },
        parsesTo(Program.parser, "x=y;") { s =>
          Program(
            elementList[Assignment](eofSpan(s))(
              assign(s, "x", atomExpr(ref(s, "y")), semiAt = 3),
            ),
          )
        },
        parsesTo(Program.parser, "x=y+1;y=x*2;") { s =>
          Program(
            elementList[Assignment](eofSpan(s))(
              assign(
                s,
                "x",
                Expr.Add.Bin(
                  mulNext(ref(s, "y")),
                  AddOp("+", spanOf(s, "+")),
                  addNext(mulNext(lit(s, "1"))),
                ),
                semiAt = 5,
              ),
              Assignment(
                Ident("y", spanOf(s, "y", from = 6)),
                `=`("=", spanOf(s, "=", from = 6)),
                boxed(
                  addNext(
                    Expr.Mul.Bin(
                      ref(s, "x", from = 6),
                      MulOp("*", spanOf(s, "*")),
                      mulNext(lit(s, "2")),
                    ),
                  ),
                ),
                `;`(";", span(s, 11, 12)),
              ),
            ),
          )
        },
        parsesTo(Program.parser, "ans=-5;") { s =>
          Program(
            elementList[Assignment](eofSpan(s))(
              assign(s, "ans", atomExpr(lit(s, "-5")), semiAt = 6),
            ),
          )
        },
        // Nested GenericBox[Expr] Empty branch: `name = _;`
        parsesTo(Program.parser, "x=_;") { s =>
          Program(
            elementList[Assignment](eofSpan(s))(
              Assignment(
                Ident("x", spanOf(s, "x")),
                `=`("=", spanOf(s, "=")),
                GenericBox.Empty(Underscore("_", span(s, 2, 3))),
                `;`(";", span(s, 3, 4)),
              ),
            ),
          )
        },
      ),
      suite("invalid")(
        failsToParse(Program.parser, "x="),
        failsToParse(Program.parser, "x=;"),
        failsToParse(Program.parser, "=1;"),
        failsToParse(Program.parser, "x=1"),
        failsToParse(Program.parser, "x=1+;"),
        failsToParse(Program.parser, "x=1*;"),
        failsToParse(Program.parser, "x=(1+2;"),
        failsToParse(Program.parser, "x=1+2);"),
        failsToParse(Program.parser, "1=2;"),
        failsToParse(Program.parser, "x=1 2;"),
        failsToParse(Program.parser, "x==1;"),
      ),
      suite("generic sum - GenericBox standalone")(
        // GenericBox[Ident]: Has forwards Ident, Empty is `_` (Nothing)
        parsesTo(GenericBox.parserIdent, "foo") { s =>
          GenericBox.Has(Ident("foo", spanOf(s, "foo")))
        },
        parsesTo(GenericBox.parserIdent, "_") { s =>
          GenericBox.Empty(Underscore("_", span(s, 0, 1)))
        },
        // GenericBox[IntLit]: Has forwards IntLit, Empty is `_`
        parsesTo(GenericBox.parserIntLit, "123") { s =>
          GenericBox.Has(IntLit("123", spanOf(s, "123"), BigInt("123")))
        },
        parsesTo(GenericBox.parserIntLit, "_") { s =>
          GenericBox.Empty(Underscore("_", span(s, 0, 1)))
        },
        parsesTo(GenericBox.parserIdent, "bar") { s =>
          GenericBox.Has(Ident("bar", spanOf(s, "bar")))
        },
      ),
    )

}
