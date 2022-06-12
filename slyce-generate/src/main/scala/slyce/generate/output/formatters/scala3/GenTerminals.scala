package slyce.generate.output.formatters.scala3

import cats.data.NonEmptyList
import cats.syntax.either.*
import klib.utils.{given, *}

import slyce.generate.grammar.*
import slyce.generate.output.*
import slyce.generate.output.formatters.scala3.GenUtils.*

private[scala3] object GenTerminals {

  def idtStr(
      utils: GenUtils,
      ts: Set[Extras.Terminal],
  ): IndentedString = {
    val (terminals, rawTerminals) =
      ts.partitionMap { term =>
        term.name match {
          case t: ExpandedGrammar.Identifier.Term.Terminal => (t.name, term).asLeft
          case r: ExpandedGrammar.Identifier.Term.Raw      => (r.name, term).asRight
        }
      }

    val terminalIdtStrs: IndentedString =
      IndentedString.inline(
        terminals.toList.sortBy(_._1).map { (n, t) =>
          makeTok(utils, n, false, t.withs)
        },
      )

    lazy val sortedRawTerminals = rawTerminals.toList.sortBy(_._1)

    lazy val rawTerminalIdtStrs: IndentedString =
      IndentedString.inline(
        sortedRawTerminals.map { (n, t) =>
          makeTok(utils, n, true, t.withs)
        },
      )

    lazy val findRawTerminalIdtStr: IndentedString =
      IndentedString.inline(
        s"val $FindRawTerminalName: $CorePath.Span.Highlight => PartialFunction[_root_.scala.Predef.String, ${utils.qualifiedPath}.Terminal] =",
        IndentedString.indented(
          "span => {",
          IndentedString.indented(
            sortedRawTerminals.map { (n, _) =>
              val name =
                if (n == "?") "`\\\\?`"
                else n.unesc("`")
              s"case ${n.unesc} => Terminal.$name(span)"
            },
          ),
          "}",
        ),
      )

    IndentedString.inline(
      s"sealed abstract class Terminal(final val tokName: _root_.scala.Predef.String) extends $CorePath.Token",
      "object Terminal {",
      IndentedString.indented(
        terminalIdtStrs,
        Option.when(rawTerminals.nonEmpty)(
          IndentedString.inline(
            IndentedString.Break,
            rawTerminalIdtStrs,
            IndentedString.Break,
            findRawTerminalIdtStr,
          ),
        ),
      ),
      "}",
    )
  }

  def makeTok(
      utils: GenUtils,
      baseTokName: String,
      isRaw: Boolean,
      ws: Option[NonEmptyList[Extras.With]],
  ): IndentedString = {
    val className =
      if (isRaw)
        if (baseTokName == "?") "`\\\\?`"
        else baseTokName.unesc("`")
      else baseTokName
    val tokName = if (isRaw) baseTokName.unesc("\"\"\"\"") else baseTokName.unesc
    val params = if (isRaw) s"span: $CorePath.Span.Highlight" else s"text: _root_.scala.Predef.String, span: $CorePath.Span.Highlight"
    val body = s"final case class $className($params)"
    val idtStr = " " * body.length
    val extWiths = ws.fold(List.empty[Extras.With])(_.toList).map { w => s"${utils.qualifiedIdentifierName(w.nt)}.${w.withType}" }.sorted
    val allWiths = if (isRaw) s"$CorePath.Token.Const" :: extWiths else extWiths

    IndentedString.inline(
      s"$body extends ${utils.qualifiedPath}.Terminal($tokName)",
      allWiths.map { w => s"$idtStr with $w" },
    )
  }

}
