package slyce.generate.output.formatters.scala3

import klib.utils.{given, *}
import klib.utils.IndentedString

import slyce.core.*
import slyce.generate.lexer.*
import slyce.generate.output.*
import slyce.generate.output.formatters.scala3.GenUtils.*

private[scala3] object GenLexer {

  def idtStr(
      utils: GenUtils,
      dfa: DFA,
  ): IndentedString =
    IndentedString.inline(
      s"val lexer: $ParsePath.Lexer[${utils.qualifiedPath}.Terminal] = {",
      IndentedString.indented(
        dfa.states.map { state =>
          IndentedString.inline(
            lexerState(utils, state),
            IndentedString.Break,
          )
        },
        s"$ParsePath.Lexer[${utils.qualifiedPath}.Terminal](state0)",
      ),
      "}",
    )

  private def lexerState(
      utils: GenUtils,
      state: DFA.State,
  ): IndentedString =
    IndentedString.inline(
      s"lazy val state${state.id} =",
      IndentedString.indented(
        s"$ParsePath.Lexer.State.fromMap[${utils.qualifiedPath}.Terminal](",
        IndentedString.indented(
          s"id = ${state.id},",
          lexerOn(state),
          state.yields match {
            case Some((_, yields)) =>
              IndentedString.inline(
                "yields = Some(",
                IndentedString.indented(
                  lexerYields(utils, yields),
                ),
                "),",
              )
            case None => "yields = None,"
          },
        ),
        ")",
      ),
    )

  private def lexerOn(
      state: DFA.State,
  ): IndentedString =
    IndentedString.inline(
      // TODO (KR) : Have more options for what this is, try to be as efficient as possible
      s"on = _root_.scala.collection.immutable.Map(",
      IndentedString.indented(
        state.transitions.toList
          .flatMap { case (chars, toState) =>
            chars.toList.map((_, toState))
          }
          .sortBy(_._1)
          .map { case (char, toState) =>
            toState match {
              case Some(toState) => s"${char.toInt} -> _root_.scala.Some(state${toState.value.id}), // ${char.unesc}"
              case None          => s"${char.toInt} -> _root_.scala.None, // ${char.unesc}"
            }
          },
      ),
      state.elseTransition match {
        case Some(to) => s").withDefaultValue(_root_.scala.Some(state${to.value.id})),"
        case None     => "),"
      },
    )

  private def lexerYields(
      utils: GenUtils,
      yields: Yields[Lazy[DFA.State]],
  ): IndentedString = {
    val toMode =
      yields.toMode.value match {
        case Yields.ToMode.Same       => s"$ParsePath.Lexer.ToMode.Same"
        case Yields.ToMode.To(mode)   => s"$ParsePath.Lexer.ToMode.To(state${mode.value.id})"
        case Yields.ToMode.Push(mode) => s"$ParsePath.Lexer.ToMode.Push(state${mode.value.id})"
        case Yields.ToMode.Pop        => s"$ParsePath.Lexer.ToMode.Pop"
      }

    IndentedString.inline(
      s"$ParsePath.Lexer.Yields(",
      IndentedString.indented(
        "yields = _root_.scala.collection.immutable.List(",
        IndentedString.indented(
          yields.yields.map { y =>
            lexerYield(utils, y.value)
          },
        ),
        "),",
        s"toMode = $toMode,",
      ),
      "),",
    )
  }

  private def lexerYield(
      utils: GenUtils,
      y: Yields.Yield,
  ): IndentedString = {
    val (span: (Option[Int], Option[Int]), build: String) =
      y match {
        case Yields.Yield.Text(subString) =>
          (
            subString,
            s"${utils.qualifiedPath}.Lexer.Terminal.$FindRawTerminalName",
          )
        case Yields.Yield.Terminal(name, text, subString) =>
          (
            subString,
            text match {
              case Some(text) =>
                s"span => _ => $ParsePath.Terminal.$name(${text.unesc}, span)"
              case None =>
                s"span => text => $ParsePath.Terminal.$name(text, span)"
            },
          )
        case Yields.Yield.ConstText(text, subString) =>
          (
            subString,
            s"span => _ => ${utils.qualifiedPath}.Lexer.Terminal.${text.unesc("`")}(span)",
          )
      }

    IndentedString.inline(
      s"$ParsePath.Lexer.Yields.Yield(",
      IndentedString.indented(
        s"span = (${span._1.getOrElse(0)}, ${span._2.getOrElse(-1)}),",
        s"build = $build,",
      ),
      "),",
    )
  }

}
