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

  // How many parse-table states each generated thunk method emits; kept small so no single chunk method
  // approaches the JVM's 64KB method-size limit (see the state-emission split below).
  private val stateChunkSize: Int = 16

  def derivedImpl[A: Type](
      maxLookAheadExpr: Expr[Int],
  )(using Quotes): Expr[Parser[A]] = {
    val maxLookAhead: Int =
      maxLookAheadExpr.evalOption.getOrElse { report.errorAndAbort("Requires int constant", maxLookAheadExpr) }

    val cache: ExtractedTypeCache = ExtractedTypeCache.empty
    val root: ExtractedType = cache.getOrCreate(Position.ofMacroExpansion)(TypeRepr.of[A])

    // Surface regex clashes on sum alternatives still hard-fail (not fixable by grammar rewrite).
    GrammarValidity.assertValid(root, cache)

    val extracted0 = FromExtractedType(root, cache, maxLookAhead)
    // Grammar-level rewrite (FIRST conflicts, unit-NT peel, left-factor seq). See GrammarRewrite.
    val extracted = GrammarRewrite(extracted0)

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

    def productInstance(typeLabel: String, fieldExprs: List[Expr[?]]): Expr[?] = {
      val prod = extracted.products.getOrElse(
        typeLabel,
        report.errorAndAbort(s"Missing product type for reduce $typeLabel"),
      )
      type T
      given Type[T] = prod.gen.typeRepr.asTypeOf
      val gen = ProductGeneric.CaseClassGeneric.of[T]
      if gen.fields.toList.size != fieldExprs.size then
        report.errorAndAbort(
          s"Arity mismatch for $typeLabel: gen=${gen.fields.toList.size} sources=${fieldExprs.size}",
        )
      gen.instantiate.fieldsToInstance(fieldExprs)
    }

    def fieldSourceExpr(
        src: FromExtractedType.FieldSource,
        args: Expr[IArray[Any]],
        fieldTpe: TypeRepr,
    ): Expr[?] = {
      def asField(e: Expr[Any]): Expr[?] = {
        type F
        given Type[F] = fieldTpe.asTypeOf
        '{ $e.asInstanceOf[F] }
      }

      src match {
        case FromExtractedType.FieldSource.Arg(idx) =>
          asField('{ $args(${ Expr(idx) }) })

        case FromExtractedType.FieldSource.FactoredList(idx) =>
          asField('{ $args(${ Expr(idx) }).asInstanceOf[FactoredSeq].list })

        case FromExtractedType.FieldSource.FactoredTrail(idx) =>
          asField('{ $args(${ Expr(idx) }).asInstanceOf[FactoredSeq].trail })

        case FromExtractedType.FieldSource.Product1(typeLabel, inner) =>
          val prod = extracted.products.getOrElse(
            typeLabel,
            report.errorAndAbort(s"Missing product for Product1 wrap $typeLabel"),
          )
          val innerField = prod.gen.fields.toList match {
            case f :: Nil => f
            case _        =>
              report.errorAndAbort(s"Product1 wrap $typeLabel must have exactly one field")
          }
          val innerExpr = fieldSourceExpr(inner, args, innerField.typeRepr)
          productInstance(typeLabel, List(innerExpr))
      }
    }

    def reduceExpr(nt: String, idx: Int): Expr[(IArray[Any], Source, Int) => Any] = {
      val kind = extracted.reduces.getOrElse(
        (nt, idx),
        report.errorAndAbort(s"Missing reduce kind for $nt/$idx"),
      )
      kind match {
        case FromExtractedType.ReduceKind.Product(typeLabel, sources) =>
          val prod = extracted.products.getOrElse(
            typeLabel,
            report.errorAndAbort(s"Missing product type for reduce $typeLabel"),
          )
          type T
          given Type[T] = prod.gen.typeRepr.asTypeOf
          val gen = ProductGeneric.CaseClassGeneric.of[T]
          val fields = gen.fields.toList
          if fields.size != sources.size then
            report.errorAndAbort(
              s"Arity mismatch for $typeLabel: gen=${fields.size} sources=${sources.size}",
            )

          '{ (args: IArray[Any], src: Source, pos: Int) =>
            ${
              val fieldExprs: List[Expr[?]] =
                fields.zip(sources).map { case (field, src) =>
                  fieldSourceExpr(src, 'args, field.typeRepr)
                }
              gen.instantiate.fieldsToInstance(fieldExprs)
            }
          }

        case FromExtractedType.ReduceKind.Identity =>
          '{ (args: IArray[Any], src: Source, pos: Int) =>
            args(0)
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

        case FromExtractedType.ReduceKind.SeqEmpty =>
          '{ (args: IArray[Any], src: Source, pos: Int) =>
            val p = src.positions(math.min(pos, src.length))
            val sp = Span.Range(src, p, p)
            FactoredSeq(ElementNil(sp), ElementOption.None(sp)): Any
          }

        case FromExtractedType.ReduceKind.SeqCons(elemProductLabel, restArity) =>
          val prod = extracted.products.getOrElse(
            elemProductLabel,
            report.errorAndAbort(s"Missing elem product for SeqCons: $elemProductLabel"),
          )
          type E
          given Type[E] = prod.gen.typeRepr.asTypeOf
          val gen = ProductGeneric.CaseClassGeneric.of[E]
          val fields = gen.fields.toList
          if fields.size != restArity + 1 then
            report.errorAndAbort(
              s"SeqCons elem $elemProductLabel: gen fields=${fields.size}, expected rest+1=${restArity + 1}",
            )

          '{ (args: IArray[Any], src: Source, pos: Int) =>
            val t = args(0)
            val tail = args(1).asInstanceOf[FactoredTail]
            tail match {
              case FactoredTail.TrailOnly =>
                // Empty list sits at the start of the trail token (not after it).
                val te = t.asInstanceOf[Element]
                val at = te.span.startInclusive
                val sp = Span.Range(src, at, at)
                FactoredSeq(ElementNil(sp), ElementOption.Some(te)): Any
              case FactoredTail.Continues(rest, cont) =>
                val elem: E = ${
                  val fieldExprs: List[Expr[?]] =
                    fields.zipWithIndex.map { case (field, fi) =>
                      type F
                      given Type[F] = field.typeRepr.asTypeOf
                      if fi == 0 then '{ t.asInstanceOf[F] } else '{ rest(${ Expr(fi - 1) }).asInstanceOf[F] }
                    }
                  gen.instantiate.fieldsToInstance(fieldExprs)
                }
                FactoredSeq(
                  NonEmptyElementList(elem.asInstanceOf[Element], cont.list),
                  cont.trail,
                ): Any
            }
          }

        case FromExtractedType.ReduceKind.SeqTailTrail =>
          '{ (args: IArray[Any], src: Source, pos: Int) =>
            FactoredTail.TrailOnly: Any
          }

        case FromExtractedType.ReduceKind.SeqTailMore(restArity) =>
          '{ (args: IArray[Any], src: Source, pos: Int) =>
            val rest = IArray.tabulate(${ Expr(restArity) })(i => args(i))
            val cont = args(${ Expr(restArity) }).asInstanceOf[FactoredSeq]
            FactoredTail.Continues(rest, cont): Any
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

    // The parse table can have many states, each with a deeply-nested look-ahead tree (worse at higher k).
    // Emitting them all as one `IArray(...)` varargs puts the whole table in a single method (the enclosing
    // object's static initializer), which readily exceeds the JVM's 64KB method-size limit. Split the states
    // across many small thunk methods (one lambda per chunk) and flatten at runtime, so no single method is
    // large. Chunk size is deliberately small so an individual chunk method stays well under the limit.
    val chunkThunks: List[Expr[() => List[ParserImpl.State]]] =
      stateExprs
        .grouped(stateChunkSize)
        .toList
        .map(group => '{ () => List(${ Varargs(group) }*) })
    val statesArr: Expr[IArray[ParserImpl.State]] =
      '{ IArray.from(List(${ Varargs(chunkThunks) }*).flatMap(_.apply())) }

    '{
      new ParserImpl[A](
        startState = 0,
        states = $statesArr,
        terminals = $termsArr,
      )
    }
  }

}
