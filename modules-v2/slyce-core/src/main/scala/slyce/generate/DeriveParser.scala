package slyce.generate

import java.util.regex.Pattern
import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

import slyce.core.{Position as _, *}
import slyce.core.builtIn.*
import slyce.generate.grammar.*
import slyce.parse.*

private[slyce] object DeriveParser {

  def derivedImpl[A: Type](
      maxLookAheadExpr: Expr[Int],
  )(using Quotes): Expr[Parser[A]] = {
    val maxLookAhead: Int =
      maxLookAheadExpr.evalOption.getOrElse { report.errorAndAbort("Requires int constant", maxLookAheadExpr) }

    val cache: ExtractedTypeCache = ExtractedTypeCache.empty
    val root: ExtractedType = cache.getOrCreate(Position.ofMacroExpansion)(TypeRepr.of[A])

    val extracted = FromExtractedType(root, cache, maxLookAhead)
    val table = ParsingTable.fromExpandedGrammar(extracted.grammar) match {
      case Right(t)  => t
      case Left(err) => report.errorAndAbort(s"Failed to build parsing table:\n$err")
    }

    val termLabels: List[String] = extracted.terminals.keys.toList.sorted
    val termIndex: Map[String, Int] = termLabels.zipWithIndex.toMap

    val terminalExprs: List[Expr[ParserImpl.Terminal]] =
      termLabels.map { lab =>
        val pt = extracted.terminals(lab)
        val patternStr = pt.regex.regexText
        type T
        given Type[T] = pt.gen.typeRepr.asTypeOf
        val buildExpr: Expr[(String, Span.Range) => Either[String, Any]] = {
          // pt.build is Expr[BuildTerminal[pt.gen.AType]] — cast via Type[T]
          val bt = pt.build.asExprOf[BuildTerminal[T]]
          '{ (text: String, span: Span.Range) => $bt.build(text, span).map(v => v: Any) }
        }
        '{
          ParserImpl.Terminal(
            name = ${ Expr(lab) },
            pattern = Pattern.compile(${ Expr(patternStr) }),
            build = $buildExpr,
          )
        }
      }

    val arityOf: Map[(String, Int), Int] =
      extracted.grammar.rawNTs.flatMap { nt =>
        nt.productions.toList.zipWithIndex.map { case (prod, idx) =>
          (nt.name.label, idx) -> prod.elements.size
        }
      }.toMap

    def reduceExpr(nt: String, idx: Int): Expr[(IArray[Any], Source, Int) => Any] = {
      val kind = extracted.reduces.getOrElse(
        (nt, idx),
        report.errorAndAbort(s"Missing reduce kind for $nt/$idx"),
      )
      kind match {
        case FromExtractedType.ReduceKind.Product(typeLabel, arity) =>
          val prod = extracted.products.getOrElse(
            typeLabel,
            report.errorAndAbort(s"Missing product type for reduce $typeLabel"),
          )
          type T
          given Type[T] = prod.gen.typeRepr.asTypeOf
          val gen = ProductGeneric.CaseClassGeneric.of[T]
          val fields = gen.fields.toList
          if fields.size != arity then
            report.errorAndAbort(s"Arity mismatch for $typeLabel: gen=${fields.size} table=$arity")

          '{ (args: IArray[Any], src: Source, pos: Int) =>
            ${
              val fieldExprs: List[Expr[?]] =
                fields.zipWithIndex.map { case (field, i) =>
                  type F
                  given Type[F] = field.typeRepr.asTypeOf
                  '{ args(${ Expr(i) }).asInstanceOf[F] }
                }
              gen.instantiate.fieldsToInstance(fieldExprs)
            }
          }

        case FromExtractedType.ReduceKind.ListNil =>
          '{ (args: IArray[Any], src: Source, pos: Int) =>
            val p = src.positions(math.min(pos, src.length))
            ElementNil(Span.Range(src, p, p)): Any
          }

        case FromExtractedType.ReduceKind.ListCons =>
          '{ (args: IArray[Any], src: Source, pos: Int) =>
            val head = args(0).asInstanceOf[Element]
            val tail = args(1).asInstanceOf[ElementList[Element]]
            NonEmptyElementList(head, tail): Any
          }

        case FromExtractedType.ReduceKind.OptSome =>
          '{ (args: IArray[Any], src: Source, pos: Int) =>
            ElementOption.Some(args(0).asInstanceOf[Element]): Any
          }

        case FromExtractedType.ReduceKind.OptNone =>
          '{ (args: IArray[Any], src: Source, pos: Int) =>
            val p = src.positions(math.min(pos, src.length))
            ElementOption.None(Span.Range(src, p, p)): Any
          }
      }
    }

    def convertAction(action: ParsingTable.Action): Expr[ParserImpl.Action] =
      action match {
        case ParsingTable.Action.Accept =>
          '{ ParserImpl.Action.Accept }
        case ParsingTable.Action.Shift(to) =>
          '{ ParserImpl.Action.Shift(${ Expr(to) }) }
        case ParsingTable.Action.Reduce(nt, prodIdx) =>
          val pop = arityOf.getOrElse((nt.label, prodIdx), report.errorAndAbort(s"No arity for ${nt.label}/$prodIdx"))
          val build = reduceExpr(nt.label, prodIdx)
          '{
            ParserImpl.Action.Reduce(
              nt = ${ Expr(nt.label) },
              prodIdx = ${ Expr(prodIdx) },
              pop = ${ Expr(pop) },
              build = $build,
            )
          }
        case ParsingTable.Action.LookAhead(onTerm, onEOF) =>
          val pairs: List[Expr[(Int, ParserImpl.Action)]] =
            onTerm.toList.map { case (term, act) =>
              val idx = termIndex.getOrElse(term.label, report.errorAndAbort(s"Unknown terminal in table: ${term.label}"))
              '{ (${ Expr(idx) }, ${ convertAction(act) }) }
            }
          val eofExpr: Expr[Option[ParserImpl.Action]] =
            onEOF match {
              case None    => '{ None }
              case Some(a) => '{ Some(${ convertAction(a) }) }
            }
          '{
            ParserImpl.Action.LookAhead(
              onTerm = Map(${ Varargs(pairs) }*),
              onEOF = $eofExpr,
            )
          }
      }

    val stateExprs: List[Expr[ParserImpl.State]] =
      table.states.map { st =>
        val gotoPairs: List[Expr[(String, Int)]] =
          st.goto.toList.map { case (nt, to) => '{ (${ Expr(nt.label) }, ${ Expr(to) }) } }
        val laExpr: Expr[ParserImpl.Action] = convertAction(st.lookAhead)
        '{
          ParserImpl.State(
            id = ${ Expr(st.id) },
            goto = Map(${ Varargs(gotoPairs) }*),
            lookAhead = $laExpr,
          )
        }
      }

    val termsArr: Expr[IArray[ParserImpl.Terminal]] =
      '{ IArray(${ Varargs(terminalExprs) }*) }
    val statesArr: Expr[IArray[ParserImpl.State]] =
      '{ IArray(${ Varargs(stateExprs) }*) }

    '{
      new ParserImpl[A](
        startState = 0,
        states = $statesArr,
        terminals = $termsArr,
      )
    }
  }

}
