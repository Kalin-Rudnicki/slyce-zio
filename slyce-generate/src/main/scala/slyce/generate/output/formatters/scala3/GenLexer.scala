package slyce.generate.output.formatters.scala3

import klib.utils.{given, *}
import klib.utils.IndentedString

import slyce.core.*
import slyce.generate.groupChars
import slyce.generate.lexer.*
import slyce.generate.output.*
import slyce.generate.output.formatters.scala3.GenUtils.*

private[scala3] object GenLexer {

  def idtStr(
      utils: GenUtils,
      dfa: DFA,
  ): IndentedString =
    IndentedString.inline(
      dfa.states.map { state =>
        IndentedString.inline(
          lexerState(utils, state),
          IndentedString.Break,
        )
      },
      s"override val lexer: $ParsePath.Lexer[${utils.qualifiedPath}.Terminal] =",
      IndentedString.indented(
        s"$ParsePath.Lexer[${utils.qualifiedPath}.Terminal](lexerState0)",
      ),
    )

  private def lexerState(
      utils: GenUtils,
      state: DFA.State,
  ): IndentedString =
    if (state.transitions.isEmpty) lexerStateDefault(utils, state)
    else {
      val grouped: List[(Either[Char, (Char, Char)], Int, Option[Lazy[DFA.State]])] =
        state.transitions.toList
          .flatMap { case (chars, to) =>
            chars.groupChars.map { either =>
              (
                either,
                either match {
                  case Left(_)             => 1
                  case Right((start, end)) => end.toInt - start.toInt
                },
                to,
              )
            }
          }
          .sortBy(_._2)
          .reverse

      val sizes: List[Int] = grouped.map(_._2)
      val first2: List[Int] = sizes.take(2)
      val afterThat: List[Int] = sizes.drop(2)

      if (sizes.size <= 3 || first2.sum >= afterThat.sum) lexerStatePF(utils, state, grouped)
      else lexerStateMap(utils, state)
    }

  private def lexerStateDefault(
      utils: GenUtils,
      state: DFA.State,
  ): IndentedString =
    IndentedString.inline(
      s"private lazy val lexerState${state.id}: $ParsePath.Lexer.State[${utils.qualifiedPath}.Terminal] =",
      IndentedString.indented(
        s"$ParsePath.Lexer.State[${utils.qualifiedPath}.Terminal](",
        IndentedString.indented(
          s"id = ${state.id},",
          state.elseTransition match {
            case Some(to) => s"on = _ => _root_.scala.Some(lexerState${to.value.id}),"
            case None     => s"on = _ => _root_.scala.None,"
          },
          assignYields(utils, state),
        ),
        ")",
      ),
    )

  private def lexerStatePF(
      utils: GenUtils,
      state: DFA.State,
      grouped: List[(Either[Char, (Char, Char)], Int, Option[Lazy[DFA.State]])],
  ): IndentedString =
    IndentedString.inline(
      s"private lazy val lexerState${state.id}: $ParsePath.Lexer.State[${utils.qualifiedPath}.Terminal] =",
      IndentedString.indented(
        s"$ParsePath.Lexer.State.fromPF[${utils.qualifiedPath}.Terminal](",
        IndentedString.indented(
          s"id = ${state.id},",
          assignYields(utils, state),
        ),
        ") {",
        IndentedString.indented(
          grouped.map { case (either, _, to) =>
            val (matchStr, comment) =
              either match {
                case Left(c) =>
                  (c.toInt.toString, c.unesc)
                case Right((c1, c2)) =>
                  (s"c if c >= ${c1.toInt} && c <= ${c2.toInt}", s"${c1.unesc} - ${c2.unesc}")
              }

            val toStr =
              to match {
                case Some(to) => s"_root_.scala.Some(lexerState${to.value.id})"
                case None     => "_root_.scala.None"
              }

            s"case $matchStr => $toStr // $comment"
          },
          state.elseTransition.map { to =>
            s"case _ => _root_.scala.Some(lexerState${to.value.id})"
          },
        ),
        "}",
      ),
    )

  private def lexerStateMap(
      utils: GenUtils,
      state: DFA.State,
  ): IndentedString =
    IndentedString.inline(
      s"private lazy val lexerState${state.id}: $ParsePath.Lexer.State[${utils.qualifiedPath}.Terminal] =",
      IndentedString.indented(
        s"$ParsePath.Lexer.State.fromMap[${utils.qualifiedPath}.Terminal](",
        IndentedString.indented(
          s"id = ${state.id},",
          s"on = _root_.scala.collection.immutable.Map(",
          IndentedString.indented(
            state.transitions.toList
              .flatMap { case (chars, toState) =>
                chars.toList.map((_, toState))
              }
              .sortBy(_._1)
              .map { case (char, toState) =>
                toState match {
                  case Some(toState) => s"${char.toInt} -> _root_.scala.Some(lexerState${toState.value.id}), // ${char.unesc}"
                  case None          => s"${char.toInt} -> _root_.scala.None, // ${char.unesc}"
                }
              },
          ),
          "),",
          state.elseTransition match {
            case Some(to) => s"elseOn = _root_.scala.Some(lexerState${to.value.id}),"
            case None     => s"elseOn = _root_.scala.None,"
          },
          assignYields(utils, state),
        ),
        ")",
      ),
    )

  private def assignYields(
      utils: GenUtils,
      state: DFA.State,
  ): IndentedString =
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
    }

  private def lexerYields(
      utils: GenUtils,
      yields: Yields[Lazy[DFA.State]],
  ): IndentedString = {
    val toMode =
      yields.toMode.value match {
        case Yields.ToMode.Same       => s"$ParsePath.Lexer.Yields.ToMode.Same"
        case Yields.ToMode.To(mode)   => s"$ParsePath.Lexer.Yields.ToMode.To($CorePath.Lazy(lexerState${mode.value.id}))"
        case Yields.ToMode.Push(mode) => s"$ParsePath.Lexer.Yields.ToMode.Push($CorePath.Lazy(lexerState${mode.value.id}))"
        case Yields.ToMode.Pop        => s"$ParsePath.Lexer.Yields.ToMode.Pop"
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
            s"${utils.qualifiedPath}.Terminal.$FindRawTerminalName",
          )
        case Yields.Yield.Terminal(name, text, subString) =>
          (
            subString,
            text match {
              case Some(text) =>
                s"span => _ => ${utils.qualifiedPath}.Terminal.$name(${text.unesc}, span)"
              case None =>
                s"span => text => ${utils.qualifiedPath}.Terminal.$name(text, span)"
            },
          )
        case Yields.Yield.ConstText(text, subString) =>
          (
            subString,
            s"span => _ => ${utils.qualifiedPath}.Terminal.${text.unesc("`")}(span)",
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
