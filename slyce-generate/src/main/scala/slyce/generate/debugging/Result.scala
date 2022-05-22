package slyce.generate.debugging

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.list.*
import java.util.UUID
import klib.utils.*
import scalatags.Text.all.{id as htmlId, *}

import slyce.core.*
import slyce.generate.*
import slyce.generate.grammar.*
import slyce.generate.lexer.*

final case class Result(
    lexer: LexerInput,
    nfa: Validated[NFA],
    dfa: Validated[DFA],
    grammar: GrammarInput,
    expandedGrammar: ExpandedGrammar2,
    originalExpandedGrammar: Validated[ExpandedGrammar],
    deDuplicatedExpandedGrammar: Validated[ExpandedGrammar],
    parsingTable: Validated[ParsingTable],
)
object Result {

  def build(
      lexer: LexerInput,
      grammar: GrammarInput,
  ): Result = {
    def unableToBuild(thing: String): Validated[Nothing] =
      Marked(s"Unable to attempt building '$thing'", Span.Unknown).leftNel

    extension [R](r: R)
      def attemptToBuild[R1, R2](nextName: String, buildWith: R => Validated[R1])(build: R1 => Validated[R2])(implicit zip: zio.Zippable[R, Validated[R2]]): zip.Out =
        buildWith(r) match {
          case Right(value) => zip.zip(r, build(value))
          case Left(_)      => zip.zip(r, unableToBuild(nextName))
        }

    val (nfa, dfa) =
      NFA
        .fromLexer(lexer)
        .attemptToBuild("dfa", identity)(DFA.fromNFA(_))

    val expandedGrammar = ExpandedGrammar2.fromGrammar(grammar)

    val (originalExpandedGrammar, deDuplicatedExpandedGrammar, parsingTable) =
      ExpandedGrammar
        .fromGrammar(grammar)
        .attemptToBuild("deDuplicatedExpandedGrammar", identity)(ExpandedGrammar.deDuplicate(_).asRight)
        .attemptToBuild("parsingTable", _._2)(ParsingTable.fromExpandedGrammar(_))

    Result(
      lexer = lexer,
      nfa = nfa,
      dfa = dfa,
      grammar = grammar,
      expandedGrammar = expandedGrammar,
      originalExpandedGrammar = originalExpandedGrammar,
      deDuplicatedExpandedGrammar = deDuplicatedExpandedGrammar,
      parsingTable = parsingTable,
    )
  }

  object resultToHTML {

    def apply(result: Result): Frag =
      html(
        head(
          tag("style")(
            shared.cssSection(".hidden")(
              display -> "none",
            ),
            shared.cssSection("td ul")(
              paddingLeft -> "20px",
            ),
            shared.cssSection("td ol")(
              paddingLeft -> "20px",
            ),
          ),
        ),
        body(
          h1("Result"),
          lexerSection(result),
          shared.verticalSpace,
          grammarSection(result),
        ),
      )

    extension (self: Long)
      private def pluralizeOn(base: String, pluralSuffix: String = "s", singularSuffix: String = ""): String =
        s"$self ${base.pluralize(self, pluralSuffix, singularSuffix)}"

    extension (str: String)
      private def pluralize(amount: Long, pluralSuffix: String = "s", singularSuffix: String = ""): String =
        s"$str${if (amount == 1) singularSuffix else pluralSuffix}"

    private object shared {

      def verticalSpace: Frag = div(height := "1px")

      def todo: Frag = h2(color := "red")("TODO")

      def section(name: String, startsHidden: Boolean = true)(bodies: Modifier*): Frag = {
        val sectionId = UUID.randomUUID.toString
        div(border := "2px solid black", margin := "10px 0px")(
          span(
            marginLeft := "15px",
            marginTop := "-12px",
            display := "inline-block",
            fontSize := "15pt",
            background := "white",
            padding := "0px 10px",
            border := "2px solid black",
            borderRadius := "10px",
            cursor := "pointer",
            css("user-select") := "none",
            attr("onclick") := s"""document.getElementById("$sectionId").classList.toggle("hidden");""",
          )(name),
          div(htmlId := sectionId, padding := "5px 10px", Option.when(startsHidden)(`class` := "hidden"))(bodies),
        )
      }

      def either[A](a: Validated[A])(onRight: A => Frag): Frag =
        a match {
          case Right(value) =>
            onRight(value)
          case Left(errors) =>
            div(
              ul(
                errors.toList.map { e =>
                  li(e.toString)
                },
              ),
            )
        }

      def eitherSection[A](name: String, a: Validated[A], startsHidden: Boolean = true)(onRight: A => Frag): Frag =
        section(name, startsHidden)(
          either(a)(onRight),
        )

      def frag(frags: Frag*): Frag = frags

      def makeTable(headers: (String, Int)*): ConcreteHtmlTag[String] =
        table(border := "1px solid black")(
          tr(
            headers.map { (l, w) =>
              makeHeaderCell(w)(l)
            },
          ),
        )

      def makeHeaderCell(w: Int): ConcreteHtmlTag[String] =
        th(width := s"${w}px", border := "1px solid black")

      def makeCell: ConcreteHtmlTag[String] =
        td(border := "1px solid black", padding := "2px 10px")

      def makeCenteredCell: ConcreteHtmlTag[String] =
        makeCell(textAlign := "center")

      def makeRow(cells: Modifier*): ConcreteHtmlTag[String] =
        tr(cells.map(makeCell(_)))

      def titledList(title: String)(items: (String, Any)*): Frag =
        frag(
          title,
          ul(margin := "0")(
            items.map { (label, value) =>
              li(s"$label: $value")
            },
          ),
        )

      def productTitleList(product: Product): Frag = {
        val simpleName = product.getClass.getSimpleName
        shared.titledList(if (simpleName.nonEmpty) simpleName else product.toString)(
          product.productElementNames.zip(product.productIterator).toList *,
        )
      }

      object autoShow {

        def apply(any: Any, maxDepth: Int = -1): Frag =
          if (maxDepth == 0)
            any.toString
          else
            any.asInstanceOf[Matchable] match {
              case anonListNT: ExpandedGrammar2.Identifier.NonTerminal.AnonListNt => showProduct(anonListNT, maxDepth - 1)
              case id: ExpandedGrammar2.Identifier                                => id.toString
              case nel: NonEmptyList[_]                                           => showList(nel.toList, maxDepth - 1)
              case list: List[_]                                                  => showList(list, maxDepth - 1)
              case product: Product                                               => showProduct(product, maxDepth - 1)
              case _                                                              => any.toString
            }

        private def showList(list: List[Any], maxDepth: Int): Frag =
          ol(
            list.map(i => li(autoShow(i, maxDepth))),
          )

        private def showProduct(product: Product, maxDepth: Int): Frag =
          frag(
            if (product.productArity == 0) product.toString else product.getClass.getSimpleName,
            ul(margin := "0")(
              product.productElementNames.zip(product.productIterator).toList.map { (k, v) =>
                li(
                  s"$k: ",
                  autoShow(v, maxDepth),
                )
              },
            ),
          )

      }

      def shadedIf(cond: Boolean): Modifier =
        Option.when(cond)(backgroundColor := "LightSlateGrey")

      def cssSection(selector: String)(properties: (scalatags.generic.Style | scalatags.generic.PixelStyle, Any)*): String =
        properties
          .map { (style, value) =>
            val cssName =
              style match {
                case style: scalatags.generic.Style      => style.cssName
                case style: scalatags.generic.PixelStyle => style.cssName
              }
            s"\n  $cssName: $value;"
          }
          .mkString(s"$selector {", "", "\n}")

    }

    private object lexerSection {

      def apply(result: Result): Frag = {
        val nfaStateMap: Map[NFA.State, Int] = allNFAStates(result.nfa)

        shared.section("Lexer", false)(
          shared.section("LexerInput")(
            // TODO (KR) :
            shared.todo,
          ),
          shared.verticalSpace,
          shared.eitherSection("NFA", result.nfa) { nfa =>
            val idToModeName: Map[Int, String] = nfa.modes.toList.map { (name, state) => (nfaStateMap(state.value.value), name) }.toMap

            shared.frag(
              nfaModeTable(nfa, nfaStateMap),
              br,
              nfaStatesTable(nfaStateMap, idToModeName),
            )
          },
          shared.verticalSpace,
          shared.eitherSection("DFA", result.dfa) { dfa =>
            val reverseStateLookup: Map[Int, DFA.NFAStates] = dfa.forDebugging.nfaStatesToState.map { (k, v) => (v.id, k) }

            shared.frag(
              dfaModeTable(dfa),
              br,
              dfaStatesTable(dfa, nfaStateMap, reverseStateLookup),
            )
          },
        )
      }

      private def allNFAStates(nfa: Validated[NFA]): Map[NFA.State, Int] =
        nfa match {
          case Right(nfa) =>
            Helpers
              .findAll(nfa.modes.map(_._2.value.value).toSet) {
                case NFA.State.TransitionOnChars(_, to) => Set(to.value)
                case NFA.State.TransitionOnEpsilon(to)  => to.map(_.value)
                case NFA.State.End(_)                   => Set.empty
              }
              .toList
              .zipWithIndex
              .toMap
          case Left(_) =>
            Map.empty
        }

      private def showNFAStates(nfaStateMap: Map[NFA.State, Int], nfaStates: Set[NFA.State]): Frag =
        ul(
          nfaStates.toList
            .map(nfaStateMap)
            .sorted
            .map { toId =>
              li(a(href := s"#nfa-state-$toId")(s"#$toId"))
            },
        )

      private def nfaModeTable(nfa: NFA, nfaStateMap: Map[NFA.State, Int]): Frag =
        shared.makeTable(
          "Mode" -> 100,
          "State" -> 100,
        )(
          nfa.modes.toList.map { (name, state) =>
            val toId = nfaStateMap(state.value.value)
            shared.makeRow(
              name,
              a(href := s"#nfa-state-$toId")(s"#$toId"),
            )
          },
        )

      private def nfaStatesTable(nfaStateMap: Map[NFA.State, Int], idToModeName: Map[Int, String]): Frag =
        shared.makeTable(
          "State" -> 75,
          "Type" -> 150,
          "Info" -> 750,
        )(
          nfaStateMap.toList.map { (state, id) =>
            tr(
              shared.makeCenteredCell(
                a(htmlId := s"nfa-state-$id")(s"#$id"),
                idToModeName.get(id).map { modeName => div(s"[$modeName]") },
              ),
              shared.makeCenteredCell(state.getClass.getSimpleName),
              shared.makeCell(nfaStateCell(nfaStateMap, state)),
            )
          },
        )

      private def nfaStateCell(nfaStateMap: Map[NFA.State, Int], state: NFA.State): Frag =
        state match {
          case NFA.State.TransitionOnChars(charClass, to) =>
            val toId = nfaStateMap(to.value)
            span(
              s"$charClass => ",
              a(href := s"#nfa-state-$toId")(s"#$toId"),
            )
          case NFA.State.End(line) =>
            ul(
              li(s"LineNo: ${line.lineNo}"),
              li(s"Regex: ${line.regex.value}"),
              li(s"ToMode: ${line.yields.toMode.value}"),
              li(
                "Yields:",
                ul(
                  line.yields.yields.map { yields =>
                    li(yields.value.toString)
                  },
                ),
              ),
            )
          case NFA.State.TransitionOnEpsilon(to) =>
            showNFAStates(nfaStateMap, to.map(_.value))
        }

      private def dfaModeTable(dfa: DFA): Frag =
        shared.makeTable(
          "Mode" -> 100,
          "State" -> 100,
        )(
          dfa.forDebugging.modeStarts.toList.map { (name, state) =>
            shared.makeRow(
              name,
              a(href := s"#dfa-state-${state.id}")(s"#${state.id}"),
            )
          },
        )

      private def dfaStatesTable(dfa: DFA, nfaStateMap: Map[NFA.State, Int], reverseStateLookup: Map[Int, DFA.NFAStates]): Frag =
        shared.makeTable(
          "State" -> 75,
          "NFA States" -> 100,
          "Transitions" -> 350,
          "Yields" -> 350,
        )(
          dfa.states.map { state =>
            shared.makeRow(
              a(htmlId := s"dfa-state-${state.id}")(s"#${state.id}"),
              showNFAStates(nfaStateMap, reverseStateLookup(state.id).toSet),
              dfaTransitions(state),
              dfaYields(state),
            )
          },
        )

      private def dfaTransitions(state: DFA.State): Frag =
        ul(
          state.transitions.toList.map { (chars, to) =>
            li(
              s"${chars.toList.sorted.map(_.unesc).mkString("[", ", ", "]")} => ",
              to match {
                case Some(to) => a(href := s"#dfa-state-${to.value.id}")(s"#${to.value.id}")
                case None     => "Ignored"
              },
            )
          },
          state.elseTransition.map { to =>
            li(
              "Else => ",
              a(href := s"#dfa-state-${to.value.id}")(s"#${to.value.id}"),
            )
          },
        )

      private def dfaYields(state: DFA.State): Frag =
        state.yields.map { (line: LexerInput.Mode.Line, yields: Yields[Lazy[DFA.State]]) =>
          ul(
            li(s"LineNo: ${line.lineNo}"),
            li(
              "ToMode: ",
              yields.toMode.value match {
                case Yields.ToMode.Same       => "Same"
                case Yields.ToMode.To(mode)   => a(href := s"#dfa-state-${mode.value.id}")(s"To(#${mode.value.id})")
                case Yields.ToMode.Push(mode) => a(href := s"#dfa-state-${mode.value.id}")(s"Push(#${mode.value.id})")
                case Yields.ToMode.Pop        => "Pop"
              },
            ),
            li(
              "Yields:",
              ul(
                yields.yields.map { yields =>
                  li(yields.value.toString)
                },
              ),
            ),
          )
        }

    }

    private object grammarSection {

      def apply(result: Result): Frag =
        shared.section("Grammer", false) {
          shared.frag(
            shared.section("GrammarInput") {
              // TODO (KR) :
              shared.frag(
                shared.todo,
              )
            },
            shared.verticalSpace,
            expandedGrammarSection("ExpandedGrammar - Original", result.originalExpandedGrammar),
            shared.verticalSpace,
            expandedGrammarSection("ExpandedGrammar - DeDuplicated", result.deDuplicatedExpandedGrammar, false),
            shared.verticalSpace,
            expandedGrammar2Section(result.expandedGrammar),
            shared.verticalSpace,
            shared.eitherSection("ParsingTable", result.parsingTable) { parsingTable =>
              // TODO (KR) :
              shared.frag(
                shared.todo,
              )
            },
          )
        }

      private def expandedGrammarSection(name: String, eg: Validated[ExpandedGrammar], startsHidden: Boolean = true): Frag =
        shared.eitherSection(name, eg, startsHidden) { eg =>
          shared.frag(
            shared.section("NTs", startsHidden)(
              egNTStatTable(eg.nts),
              br,
              egNTTable(eg),
            ),
            shared.verticalSpace,
            allIdentifiersSection(eg),
          )
        }

      private def egNTStatTable(nts: List[ExpandedGrammar.NT[ExpandedGrammar.Identifier.NonTerminal]]): Frag =
        shared.makeTable(
          "Stat" -> 150,
          "Value" -> 150,
        )(
          shared.makeRow("Total NTs", nts.size),
          shared.makeRow("Total Reductions", nts.flatMap(_.productions.toList).size),
        )

      private def egNTTable(eg: ExpandedGrammar): Frag =
        shared.makeTable(
          "NonTerminal" -> 150,
          "Idx" -> 50,
          "LiftIdx" -> 75,
          "Reductions" -> 300,
          "Aliases" -> 150,
          "Extras" -> 450,
          "Types" -> 250,
        )(
          eg.nts.map { nt =>
            val extras = eg.extras.getOrElse(nt.name, Nil)
            val aliases = eg.aliases.filter(_.actual == nt.name)
            val allNames = (nt.name :: aliases.map(_.named)).toSet
            val types = eg.withs.filter { w => allNames.contains(w.typeInNT) }.groupMap(_.`type`)(_.extendingIdentifier)

            shared.frag(
              tr(
                shared.makeCell(rowspan := nt.productions.size)(
                  shared.productTitleList(nt.name),
                  br,
                  div(fontSize := "0.75rem", textAlign := "center")(s"[${nt.productions.size.pluralizeOn("reduction")}]"),
                ),
                reductionCells(0, nt.productions.head),
                shared.makeCell(rowspan := nt.productions.size, shared.shadedIf(aliases.isEmpty))(
                  ul(
                    aliases.map { a => li(shared.productTitleList(a.named)) },
                  ),
                ),
                shared.makeCell(rowspan := nt.productions.size, shared.shadedIf(extras.isEmpty))(
                  ul(
                    extras.map { extra =>
                      li(shared.productTitleList(extra))
                    },
                  ),
                ),
                shared.makeCell(rowspan := nt.productions.size, shared.shadedIf(types.isEmpty))(
                  ul(
                    types.toList.map { (wType, ids) =>
                      li(
                        wType.toString,
                        ul(
                          ids.map { id => li(id.toString) },
                        ),
                      )
                    },
                  ),
                ),
              ),
              nt.productions.tail.zipWithIndex.map { (r, idx) =>
                tr(reductionCells(idx + 1, r))
              },
            )
          },
        )

      private def reductionCells(
          idx: Int,
          reduction: ExpandedGrammar.NT.Production,
      ): Frag =
        shared.frag(
          shared.makeCenteredCell(idx),
          shared.makeCenteredCell(shared.shadedIf(reduction.liftIdx.isEmpty))(reduction.liftIdx),
          shared.makeCell(shared.shadedIf(reduction.elements.isEmpty))(
            Option.when(reduction.elements.nonEmpty) {
              ul(
                reduction.elements.map { e =>
                  li(e.toString)
                },
              )
            },
          ),
        )

      private def allIdentifiersSection(eg: ExpandedGrammar): Frag = {
        val all: List[ExpandedGrammar.Identifier] =
          eg.nts
            .flatMap[ExpandedGrammar.Identifier] { nt =>
              nt.name :: nt.productions.toList.flatMap(_.elements)
            }
            .distinct

        shared.section(s"All Identifiers (${all.size})")(
          shared.makeTable(
            "Identifier" -> 150,
            "Extends" -> 200,
          )(
            all.sortBy(_.toString).map { id =>
              shared.makeRow(
                id.toString,
                shared.todo,
              )
            },
          ),
        )
      }

      private def expandedGrammar2Section(eg: ExpandedGrammar2): Frag =
        shared.section("ExpandedGrammar", false)(
          expandedGrammar2Nts("NTs - Initial", eg.initialNTGroups),
          shared.verticalSpace,
          expandedGrammar2Nts("NTs - DeDuplicated", eg.deDuplicatedNTGroups),
        )

      private def expandedGrammar2Nts(label: String, ntgs: List[ExpandedGrammar2.NTGroup]): Frag =
        shared.section(label, false)(
          shared.makeTable(
            "Stat" -> 150,
            "Value" -> 150,
          )(
            shared.makeRow("# NTGroups", ntgs.size),
            shared.makeRow("# RawNTs", ntgs.flatMap(_.rawNTs.toList).size),
            shared.makeRow("# Productions", ntgs.flatMap(_.rawNTs.toList.flatMap(_.productions.toList)).size),
            shared.makeRow(
              "# Identifiers",
              ntgs
                .flatMap {
                  _.rawNTs.toList.flatMap { rawNT =>
                    rawNT.name :: rawNT.productions.toList.flatMap(_.elements)
                  }
                }
                .distinct
                .size,
            ),
          ),
          br,
          shared.makeTable(
            "NT Group" -> 500,
            "Name" -> 350,
            "Production" -> 400,
          )(
            ntgs.sortBy(_.toString).map { ntg =>
              def makeRow(idx1: Int, idx2: Int, rawNT: ExpandedGrammar2.RawNT, prod: ExpandedGrammar2.Production): Frag =
                tr(
                  Option.when(idx1 == 0 && idx2 == 0)(
                    shared.makeCell(rowspan := ntg.rawNTs.flatMap(_.productions).size)(
                      shared.autoShow(ntg, -1),
                    ),
                  ),
                  Option.when(idx2 == 0)(
                    shared.makeCell(rowspan := rawNT.productions.size)(
                      shared.autoShow(rawNT.name),
                      div(fontSize := "0.75rem", textAlign := "center")(s"[${rawNT.productions.size.pluralizeOn("production")}]"),
                    ),
                  ),
                  shared.makeCell(shared.shadedIf(prod.elements.isEmpty))(
                    if (prod.elements.nonEmpty)
                      shared.autoShow(prod.elements)
                    else
                      "[Empty Production]",
                  ),
                )

              ntg.rawNTs.toList.zipWithIndex.map { (nt, idx1) =>
                nt.productions.toList.zipWithIndex.map { (prod, idx2) =>
                  makeRow(idx1, idx2, nt, prod)
                }
              }
            },
          ),
        )

    }

  }

}
