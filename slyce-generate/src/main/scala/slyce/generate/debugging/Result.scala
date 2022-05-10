package slyce.generate.debugging

import cats.syntax.either.*
import klib.utils.*
import scalatags.Text.all.{id as htmlId, *}

import slyce.core.*
import slyce.generate.*
import slyce.generate.lexer.*

final case class Result(
    lexer: LexerInput,
    nfa: Validated[NFA],
    dfa: Validated[DFA],
)
object Result {

  def build(
      lexer: LexerInput,
  ): Result = {
    val (nfa, dfa) =
      NFA.fromLexer(lexer) match {
        case right @ Right(nfa) => (right, DFA.fromNFA(nfa))
        case left @ Left(_)     => (left, Marked("Unable to attempt building DFA", Span.Unknown).leftNel)
      }

    Result(
      lexer = lexer,
      nfa = nfa,
      dfa = dfa,
    )
  }

  object resultToHTML {

    def apply(result: Result): Frag =
      html(
        head(),
        body(
          h1("Result"),
          section("Lexer") {
            val nfaStateMap: Map[NFA.State, Int] = allNFAStates(result.nfa)

            List[Frag](
              eitherSection("NFA", result.nfa) { nfa =>
                val idToModeName: Map[Int, String] = nfa.modes.toList.map { (name, state) => (nfaStateMap(state.value.value), name) }.toMap

                List[Frag](
                  nfaModeTable(nfa, nfaStateMap),
                  br,
                  nfaStatesTable(nfaStateMap, idToModeName),
                )
              },
              div(height := "1px"),
              eitherSection("DFA", result.dfa) { dfa =>
                val reverseStateLookup: Map[Int, DFA.NFAStates] = dfa.forDebugging.nfaStatesToState.map { (k, v) => (v.id, k) }

                List[Frag](
                  dfaModeTable(dfa),
                  br,
                  dfaStatesTable(dfa, nfaStateMap, reverseStateLookup),
                )
              },
            )
          },
        ),
      )

    private def section(name: String)(body: Frag): Frag =
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
        )(name),
        div(padding := "5px 10px")(body),
      )

    private def either[A](a: Validated[A])(onRight: A => Frag): Frag =
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

    private def eitherSection[A](name: String, a: Validated[A])(onRight: A => Frag): Frag =
      section(name)(
        either(a)(onRight),
      )

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
      table(border := "1px solid black")(
        tr(
          th(width := "100px", border := "1px solid black")("Mode"),
          th(width := "100px", border := "1px solid black")("State"),
        ),
        nfa.modes.toList.map { (name, state) =>
          val toId = nfaStateMap(state.value.value)
          tr(
            td(border := "1px solid black")(name),
            td(border := "1px solid black")(a(href := s"#nfa-state-$toId")(s"#$toId")),
          )
        },
      )

    private def nfaStatesTable(nfaStateMap: Map[NFA.State, Int], idToModeName: Map[Int, String]): Frag =
      table(border := "1px solid black")(
        tr(
          th(width := "75px", border := "1px solid black")("State"),
          th(width := "150px", border := "1px solid black")("Type"),
          th(width := "750px", border := "1px solid black")("Info"),
        ),
        nfaStateMap.toList.map { (state, id) =>
          tr(
            td(border := "1px solid black", textAlign := "center")(
              a(htmlId := s"nfa-state-$id")(s"#$id"),
              idToModeName.get(id).map { modeName => div(s"[$modeName]") },
            ),
            td(border := "1px solid black", textAlign := "center")(state.getClass.getSimpleName),
            td(border := "1px solid black")(
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
              },
            ),
          )
        },
      )

    private def dfaModeTable(dfa: DFA): Frag =
      table(border := "1px solid black")(
        tr(
          th(width := "100px", border := "1px solid black")("Mode"),
          th(width := "100px", border := "1px solid black")("State"),
        ),
        dfa.forDebugging.modeStarts.toList.map { (name, state) =>
          tr(
            td(border := "1px solid black")(name),
            td(border := "1px solid black")(a(href := s"#dfa-state-${state.id}")(s"#${state.id}")),
          )
        },
      )

    private def dfaStatesTable(dfa: DFA, nfaStateMap: Map[NFA.State, Int], reverseStateLookup: Map[Int, DFA.NFAStates]): Frag =
      table(border := "1px solid black")(
        tr(
          th(width := "75px", border := "1px solid black")("State"),
          th(width := "100px", border := "1px solid black")("NFA States"),
          th(width := "350px", border := "1px solid black")("Transitions"),
          th(width := "350px", border := "1px solid black")("Yields"),
        ),
        dfa.states.map { state =>
          tr(
            td(border := "1px solid black")(a(htmlId := s"dfa-state-${state.id}")(s"#${state.id}")),
            td(border := "1px solid black")(showNFAStates(nfaStateMap, reverseStateLookup(state.id).toSet)),
            td(border := "1px solid black")(
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
              ),
            ),
            td(border := "1px solid black")(
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
              },
            ),
          )
        },
      )

  }

}
