package slyce.generate.output.formatters.scala3

import java.util.UUID
import klib.utils.{given, *}

import slyce.generate.grammar.*
import slyce.generate.output.*

private[scala3] final class GenUtils(pkg: List[String], name: String, result: Result) {

  private val anonUUIDMap: Map[UUID, Int] = result.expandedGrammar.deDuplicatedNTGroups.collect { case ExpandedGrammar.NTGroup.ListNT(Right(uuid), _, _, _) => uuid }.distinct.zipWithIndex.toMap

  val qualifiedPath: String = ("_root_" :: pkg ::: name :: Nil).mkString(".")

  def identifierName(id: ExpandedGrammar.Identifier): String =
    id match {
      case terminal: ExpandedGrammar.Identifier.NonTerminal =>
        terminal match {
          case ExpandedGrammar.Identifier.NonTerminal.NamedNt(name)         => name
          case ExpandedGrammar.Identifier.NonTerminal.NamedListNtTail(name) => s"${name}Tail"
          case ExpandedGrammar.Identifier.NonTerminal.AnonListNt(key, _type) =>
            val suffix =
              _type match {
                case ExpandedGrammar.Identifier.NonTerminal.ListType.Simple => ""
                case ExpandedGrammar.Identifier.NonTerminal.ListType.Head   => "Head"
                case ExpandedGrammar.Identifier.NonTerminal.ListType.Tail   => "Tail"
              }
            s"AnonList${anonUUIDMap(key)}$suffix"
          case ExpandedGrammar.Identifier.NonTerminal.AssocNt(name, idx) => s"$name$idx"
          case ExpandedGrammar.Identifier.NonTerminal.AnonOptNt(identifier) =>
            identifier match {
              case ExpandedGrammar.Identifier.Term.Raw(name) => s"Optional_$name".unesc("`")
              case _                                         => s"Optional_${identifierName(identifier)}"
            }
        }
      case term: ExpandedGrammar.Identifier.Term =>
        term match {
          case ExpandedGrammar.Identifier.Term.Terminal(name) => name
          case ExpandedGrammar.Identifier.Term.Raw(name)      => name.unesc("`")
        }
    }

  def qualifiedIdentifierName(id: ExpandedGrammar.Identifier): String = {
    val prefix =
      id match {
        case _: ExpandedGrammar.Identifier.NonTerminal => "NonTerminal"
        case _: ExpandedGrammar.Identifier.Term        => "Terminal"
      }
    s"$qualifiedPath.$prefix.${identifierName(id)}"
  }

}
private[scala3] object GenUtils {

  val CorePath: String = "_root_.slyce.core"
  val ParsePath: String = "_root_.slyce.parse"

  val FindRawTerminalName: String = "__findRawTerminal"

  extension (idtStrs: List[IndentedString]) {
    def surroundWithBreaks: IndentedString =
      IndentedString.inline(
        idtStrs
          .foldLeft(List[IndentedString](IndentedString.Break)) { (l, s) => IndentedString.Break :: s :: l }
          .reverse,
      )
  }

}
