package slyce.generate.output.formatters.scala3

import cats.data.NonEmptyList
import cats.syntax.list.*
import cats.syntax.option.*
import klib.utils.{given, *}
import klib.utils.IndentedString

import slyce.generate.grammar.*
import slyce.generate.output.*
import slyce.generate.output.formatters.scala3.GenUtils.*

private[scala3] object GenNonTerminals {

  def idtStr(
      utils: GenUtils,
      nts: Set[Extras.NonTerminal],
  ): IndentedString = {
    val ntIdtStrs: IndentedString =
      nts.toList
        .sortBy { nt =>
          utils.identifierName(nt.name)
        }
        .map(nonTerminal(utils, _))
        .surroundWithBreaks

    IndentedString.inline(
      s"sealed abstract class NonTerminal(final val ntName: _root_.scala.Predef.String) extends $CorePath.NonTerminal",
      "object NonTerminal {",
      IndentedString.indented(
        ntIdtStrs,
      ),
      "}",
    )
  }

  private def nonTerminal(
      utils: GenUtils,
      nt: Extras.NonTerminal,
  ): IndentedString = {
    val ntName = utils.identifierName(nt.name)
    val functs = nt.ntg.flatMap(functions(utils, _).toNel).map(_.toList.surroundWithBreaks)
    val types = typeDefs(utils, nt)
    nt.prods match {
      case Extras.NonTerminal.Productions.Single(prod) =>
        IndentedString.inline(
          production(utils, prod, ntName, s"${utils.qualifiedPath}.NonTerminal(${ntName.unesc})", nt.withs, functs),
          types.map { types =>
            IndentedString.inline(
              s"object $ntName {",
              IndentedString.indented(
                IndentedString.Break,
                types,
                IndentedString.Break,
              ),
              "}",
            )
          },
        )
      case Extras.NonTerminal.Productions.Many(prods) =>
        IndentedString.inline(
          buildProd(
            utils,
            IndentedString.inline(),
            s"sealed trait $ntName",
            s"${utils.qualifiedPath}.NonTerminal(${ntName.unesc})",
            nt.withs,
            functs,
          ),
          s"object $ntName {",
          IndentedString.indented(
            types.map { types =>
              IndentedString.inline(
                IndentedString.Break,
                types,
              )
            },
            prods.toList.map { p =>
              production(utils, p.prod, s"_${p.idx + 1}", s"${utils.qualifiedPath}.NonTerminal.$ntName", nt.withs, None)
            }.surroundWithBreaks,
          ),
          "}",
        )
    }
  }

  private def buildProd(
      utils: GenUtils,
      nonLastLines: IndentedString,
      lastLineBase: String,
      baseExt: String,
      ws: Option[NonEmptyList[Extras.With]],
      functions: Option[IndentedString],
  ): IndentedString = {
    val idt = " " * lastLineBase.length
    def makeBody(functions: IndentedString): IndentedString =
      IndentedString.inline(
        IndentedString.indented(
          functions,
        ),
        "}",
      )

    val (eol: String, withs: List[String], body: IndentedString) =
      (functions, ws) match {
        case (Some(fs), Some(ws)) =>
          val ws1 = ws.map { w => s"${utils.qualifiedIdentifierName(w.nt)}.${w.withType}" }.sorted.reverse
          val ws2 = NonEmptyList(s"${ws1.head} {", ws1.tail).reverse
          ("", ws2.toList, makeBody(fs))
        case (Some(fs), None) =>
          (" {", Nil, makeBody(fs))
        case (None, Some(ws)) =>
          ("", ws.toList.map(w => s"${utils.qualifiedIdentifierName(w.nt)}.${w.withType}").sorted, IndentedString.inline())
        case (None, None) =>
          ("", Nil, IndentedString.inline())
      }

    IndentedString.inline(
      nonLastLines,
      s"$lastLineBase extends $baseExt$eol",
      withs.map { w => s"$idt with $w" },
      body,
    )
  }

  private def production(
      utils: GenUtils,
      prod: Extras.NonTerminal.Production,
      name: String,
      baseExt: String,
      ws: Option[NonEmptyList[Extras.With]],
      functions: Option[IndentedString],
  ): IndentedString =
    prod match {
      case Extras.NonTerminal.Production.CaseObject =>
        buildProd(
          utils,
          IndentedString.inline(),
          s"case object $name",
          baseExt,
          ws,
          functions,
        )
      case Extras.NonTerminal.Production.CaseClass(elements) =>
        buildProd(
          utils,
          IndentedString.inline(
            s"final case class $name(",
            IndentedString.indented(
              elements.toList.zipWithIndex.map { (e, i) =>
                s"_${i + 1}: ${utils.qualifiedIdentifierName(e)},"
              },
            ),
          ),
          ")",
          baseExt,
          ws,
          functions,
        )
    }

  private def caseParens(liftList: LiftList[Any], liftName: String, lastParam: Option[String] = None): String =
    (liftList.before.map(_ => "_") ::: liftName :: liftList.after.map(_ => "_") ::: lastParam.toList).mkString("(", ", ", ")")

  private def functions(
      utils: GenUtils,
      ntg: ExpandedGrammar.NTGroup,
  ): List[IndentedString] = {
    val ntName = utils.qualifiedIdentifierName(ExpandedGrammar.ntGroupHead(ntg))
    ntg match {
      case ExpandedGrammar.NTGroup.BasicNT(_, _) =>
        Nil
      case ExpandedGrammar.NTGroup.LiftNT(_, prods) =>
        val functionDef = s"final def lift: $ntName.${Extras.With.Type.Lift} ="
        prods match {
          case NonEmptyList(head, Nil) =>
            s"$functionDef this._${head.liftIdx + 1}" :: Nil
          case prods =>
            IndentedString.inline(
              functionDef,
              IndentedString.indented(
                "this match {",
                IndentedString.indented(
                  prods.toList.zipWithIndex.map { (prod, idx) =>
                    s"case $ntName._${idx + 1}${caseParens(prod, "lift")} => lift"
                  },
                ),
                "}",
              ),
            ) :: Nil
        }
      case ExpandedGrammar.NTGroup.ListNT(name, listType, startProds, repeatProds) =>
        val ntName2 = utils.qualifiedIdentifierName(ExpandedGrammar.listNTId(name, ExpandedGrammar.Identifier.NonTerminal.ListType.Tail))

        val (loopType: String, repeat: LiftList[ExpandedGrammar.Identifier]) =
          (listType, repeatProds) match {
            case (GrammarInput.NonTerminal.ListNonTerminal.Type.*, None)              => (ntName, startProds)
            case (GrammarInput.NonTerminal.ListNonTerminal.Type.*, Some(repeatProds)) => (ntName2, repeatProds)
            case (GrammarInput.NonTerminal.ListNonTerminal.Type.+, None)              => (ntName2, startProds)
            case (GrammarInput.NonTerminal.ListNonTerminal.Type.+, Some(repeatProds)) => (ntName2, repeatProds)
          }

        val (retTypePath, retType) =
          listType match {
            case GrammarInput.NonTerminal.ListNonTerminal.Type.* => ("_root_.scala", "List")
            case GrammarInput.NonTerminal.ListNonTerminal.Type.+ => ("_root_.cats.data", "NonEmptyList")
          }

        IndentedString.inline(
          s"final def to$retType: $retTypePath.$retType[$ntName.${Extras.With.Type.Lift}] = {",
          IndentedString.indented(
            "@_root_.scala.annotation.tailrec",
            s"def loop(queue: $loopType, stack: _root_.scala.List[$ntName.${Extras.With.Type.Lift}] =",
            IndentedString.indented(
              "queue match {",
              IndentedString.indented(
                s"case $loopType._1${caseParens(repeat, "lift", "next".some)} => loop(next, lift :: stack)",
                s"case $loopType._2 => stack.reverse",
              ),
              "}",
            ),
            IndentedString.Break,
            (listType, repeatProds) match {
              case (GrammarInput.NonTerminal.ListNonTerminal.Type.*, None) =>
                "loop(this, Nil)"
              case (GrammarInput.NonTerminal.ListNonTerminal.Type.*, Some(_)) =>
                IndentedString.inline(
                  "this match {",
                  IndentedString.indented(
                    s"case $ntName._1${caseParens(startProds, "lift", "next".some)} => loop(next, lift :: Nil)",
                    s"case $ntName._2 => Nil",
                  ),
                  "}",
                )
              case (GrammarInput.NonTerminal.ListNonTerminal.Type.+, _) =>
                s"_root_.cats.data.NonEmptyList[$ntName.${Extras.With.Type.Lift}](this._${startProds.liftIdx + 1}, loop(this._${startProds.size + 1}, Nil))"
            },
          ),
          "}",
        ) :: Nil
      case ExpandedGrammar.NTGroup.AssocNT(name, assocs, base) =>
        val retType = s"$ParsePath.Expression[$ntName.${Extras.With.Type.Operand}, $ntName.${Extras.With.Type.Operator}]"
        val baseName = utils.qualifiedIdentifierName(ExpandedGrammar.assocNTId(name, assocs.size + 1))

        val nonBaseSubFunctions: List[IndentedString] =
          assocs.toList.zipWithIndex.map { case ((_, side), idx) =>
            val myName = utils.qualifiedIdentifierName(ExpandedGrammar.assocNTId(name, idx + 1))
            IndentedString.inline(
              s"def toExpr${idx + 1}(expr: $myName): $retType =",
              IndentedString.indented(
                "expr match {",
                IndentedString.indented(
                  side match {
                    case GrammarInput.NonTerminal.AssocNonTerminal.Type.Left =>
                      s"case $myName._1(left, op, right) => $ParsePath.Expression(toExpr${idx + 1}(left), op, toExpr${idx + 2}(right))"
                    case GrammarInput.NonTerminal.AssocNonTerminal.Type.Right =>
                      s"case $myName._1(left, op, right) => $ParsePath.Expression(toExpr${idx + 2}(left), op, toExpr${idx + 1}(right))"
                  },
                  s"case $myName._2(expr) => toExpr${idx + 2}(expr)",
                ),
                "}",
              ),
            )
          }

        val baseSubFunction: IndentedString =
          IndentedString.inline(
            s"def toExpr${assocs.size + 1}(expr: $baseName): $retType =",
            IndentedString.indented(
              base match {
                case Left(_) =>
                  "$ParsePath.Expression(expr)"
                case Right(lift) =>
                  IndentedString.inline(
                    "expr match {",
                    IndentedString.indented(
                      lift.toList.zipWithIndex.map { (prod, idx) =>
                        val liftType = utils.qualifiedIdentifierName(prod.lift)
                        if (liftType == ntName) s"case $baseName._${idx + 1}${caseParens(prod, "expr")} => toExpr1(expr)"
                        else s"case $baseName._${idx + 1}${caseParens(prod, "expr")} => $ParsePath.Expression(expr)"
                      },
                    ),
                    "}",
                  )
              },
            ),
          )

        val subsFunctions: IndentedString =
          (nonBaseSubFunctions :+ baseSubFunction)
            .map(IndentedString.inline(_, IndentedString.Break))

        IndentedString.inline(
          s"final def toExpr: $retType = {",
          IndentedString.indented(
            subsFunctions,
            "toExpr1(this)",
          ),
          "}",
        ) :: Nil
      case ExpandedGrammar.NTGroup.Optional(id) =>
        IndentedString.inline(
          s"final def toOption: _root_.scala.Option[${utils.qualifiedIdentifierName(id)}] =",
          IndentedString.indented(
            "this match {",
            IndentedString.indented(
              s"case $ntName._1(lift) => _root_.scala.Some(lift)",
              s"case $ntName._2 => _root_.scala.None",
            ),
            "}",
          ),
        ) :: Nil
    }
  }

  private def typeDefs(
      utils: GenUtils,
      nt: Extras.NonTerminal,
  ): Option[IndentedString] =
    nt.definedTypes.toNel.map { definedTypes =>
      IndentedString.inline(
        definedTypes.toList.map {
          case Extras.NonTerminal.TypeDefinition.Type(n, id)   => s"type $n = ${utils.qualifiedIdentifierName(id)}"
          case Extras.NonTerminal.TypeDefinition.Trait(n, tok) => s"sealed trait $n${if (tok) s" extends $CorePath.Token" else ""}"
        },
      )
    }
}
