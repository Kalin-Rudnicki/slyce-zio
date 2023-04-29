package slyce.generate.output.formatters.scala3

import cats.syntax.list.*
import harness.core.{given, *}

import slyce.generate.grammar.*
import slyce.generate.output.formatters.scala3.GenUtils.*

private[scala3] object GenGrammar {

  def idtStr(
      utils: GenUtils,
      expandedGrammar: ExpandedGrammar,
      parsingTable: ParsingTable,
  ): IndentedString = {
    val grammarTypeArgs: String =
      s"${utils.qualifiedPath}.Terminal, ${utils.qualifiedPath}.NonTerminal, ${utils.qualifiedPath}.NonTerminal.${expandedGrammar.startNt.value}"

    IndentedString.inline(
      parsingTable.parseStates.map { state =>
        IndentedString.inline(
          parserState(
            utils,
            ExpandedGrammar.Identifier.NonTerminal.NamedNt(expandedGrammar.startNt.value),
            grammarTypeArgs,
            state,
            expandedGrammar.deDuplicatedNTGroups.flatMap(_.rawNTs.toList).map { nt => (nt.name, nt) }.toMap,
          ),
          IndentedString.Break,
        )
      },
      s"override val grammar: $ParsePath.Grammar[$grammarTypeArgs] =",
      IndentedString.indented(
        s"$ParsePath.Grammar[$grammarTypeArgs](grammarState0)",
      ),
    )
  }

  private def parserState(
      utils: GenUtils,
      startNT: ExpandedGrammar.Identifier.NonTerminal,
      grammarTypeArgs: String,
      state: ParsingTable.ParseState,
      rawNTs: Map[ExpandedGrammar.Identifier.NonTerminal, ExpandedGrammar.RawNT],
  ): IndentedString =
    IndentedString.inline(
      s"lazy val grammarState${state.id}: $ParsePath.Grammar.State[$grammarTypeArgs] =",
      IndentedString.indented(
        s"$ParsePath.Grammar.State[$grammarTypeArgs](",
        IndentedString.indented(
          s"id = ${state.id},",
          parserStateOnTerm(utils, startNT, grammarTypeArgs, state, rawNTs),
          parserStateOnNT(utils, state),
        ),
        ")",
      ),
    )

  private def parserStateOnTerm(
      utils: GenUtils,
      startNT: ExpandedGrammar.Identifier.NonTerminal,
      grammarTypeArgs: String,
      state: ParsingTable.ParseState,
      rawNTs: Map[ExpandedGrammar.Identifier.NonTerminal, ExpandedGrammar.RawNT],
  ): IndentedString =
    IndentedString.inline(
      "onTerm = {",
      IndentedString.indented(
        parserOnTermItems(utils, Nil, state.lookAhead).map { case (list, action) =>
          val matchOnStr: String = list.mkString(" :: ")
          val actionIdtStr: IndentedString =
            action match {
              case ParsingTable.ParseState.Action.Accept =>
                IndentedString.inline(
                  s"$ParsePath.Grammar.State.Action.Accept[$grammarTypeArgs] {",
                  IndentedString.indented(
                    s"case $ParsePath.Grammar.StackElement(_root_.scala.Right(root: ${utils.qualifiedIdentifierName(startNT)}), _) :: _root_.scala.Nil => root",
                  ),
                  "}",
                )
              case ParsingTable.ParseState.Action.Reduce(nt, prodNIdx) =>
                val rawNT = rawNTs(nt)
                val prod = rawNT.productions.toList(prodNIdx)

                val fqNT =
                  if (rawNT.productions.size == 1) s"${utils.qualifiedIdentifierName(nt)}"
                  else s"${utils.qualifiedIdentifierName(nt)}._${prodNIdx + 1}"

                val (
                  matchCurrent: String,
                  matchStack: IndentedString,
                  retState: String,
                  retNT: String,
                  retStack: String,
                ) =
                  prod.elements.toNel match {
                    case None =>
                      (
                        "toState",
                        "stack,": IndentedString,
                        "toState",
                        fqNT,
                        "stack",
                      )
                    case Some(elements) =>
                      def elemStr(id: ExpandedGrammar.Identifier, idx: Int): String = {
                        val side =
                          id match {
                            case _: ExpandedGrammar.Identifier.Term        => "Left"
                            case _: ExpandedGrammar.Identifier.NonTerminal => "Right"
                          }
                        val stateName =
                          if (idx == 0) "toState"
                          else "_"

                        // TODO (KR) : I think this will break if there is '1 production' with '0 elements',
                        //           : and a '.type' needs to be appended, I think.
                        s"$ParsePath.Grammar.StackElement(_root_.scala.$side(_${idx + 1}: ${utils.qualifiedIdentifierName(id)}), $stateName) ::"
                      }

                      val reversed = elements.toList.zipWithIndex.map(elemStr).reverse

                      (
                        "_",
                        IndentedString.inline(
                          reversed.head,
                          IndentedString.indented(
                            reversed.tail,
                            "stack,",
                          ),
                        ),
                        "toState",
                        s"$fqNT(${(1 to elements.size).mkString("_", ", _", "")})",
                        "stack",
                      )
                  }

                IndentedString.inline(
                  s"$ParsePath.Grammar.State.Action.Reduce[$grammarTypeArgs] {",
                  IndentedString.indented(
                    s"case (",
                    IndentedString.indented(
                      s"$matchCurrent,",
                      matchStack,
                    ),
                    s") =>",
                    IndentedString.indented(
                      s"($retState, $retNT, $retStack)",
                    ),
                  ),
                  s"}",
                )
              case ParsingTable.ParseState.Action.Push(toStateId) =>
                s"$ParsePath.Grammar.State.Action.Shift[$grammarTypeArgs](grammarState$toStateId)"
            }

          IndentedString.inline(
            s"case $matchOnStr =>",
            IndentedString.indented(
              actionIdtStr,
            ),
          )
        },
      ),
      "},",
    )

  private def parserOnTermItems(
      utils: GenUtils,
      prefix: List[String],
      lookAhead: ParsingTable.ParseState.Action.LookAhead,
  ): List[(List[String], ParsingTable.ParseState.Action.Simple)] =
    lookAhead.actionsOnTerminals.toList.flatMap { case (term, action) =>
      parserActionsOnTerminals(utils, prefix :+ s"(${if (prefix.isEmpty) "tok" else "_"}: ${utils.qualifiedIdentifierName(term)})", action)
    } :::
      lookAhead.actionOnEOF.map(parseActionOnEOF(prefix, _)).toList

  private def parserActionsOnTerminals(
      utils: GenUtils,
      prefix: List[String],
      action: ParsingTable.ParseState.Action,
  ): List[(List[String], ParsingTable.ParseState.Action.Simple)] =
    action match {
      case action: ParsingTable.ParseState.Action.EOFAction    => (prefix :+ "_", action) :: Nil
      case action: ParsingTable.ParseState.Action.Push         => (prefix :+ "_", action) :: Nil
      case lookAhead: ParsingTable.ParseState.Action.LookAhead => parserOnTermItems(utils, prefix, lookAhead)
    }

  private def parseActionOnEOF(
      prefix: List[String],
      eofAction: ParsingTable.ParseState.Action.EOFAction,
  ): (List[String], ParsingTable.ParseState.Action.Simple) =
    (prefix :+ "_root_.scala.Nil", eofAction)

  private def parserStateOnNT(
      utils: GenUtils,
      state: ParsingTable.ParseState,
  ): IndentedString =
    if (state.actionsOnNonTerminals.isEmpty)
      "onNT = PartialFunction.empty"
    else
      IndentedString.inline(
        "onNT = {",
        IndentedString.indented(
          state.actionsOnNonTerminals.toList.map { case (nt, action) =>
            s"case _: ${utils.qualifiedIdentifierName(nt)} => grammarState${action.toStateId}"
          },
        ),
        "},",
      )

}
