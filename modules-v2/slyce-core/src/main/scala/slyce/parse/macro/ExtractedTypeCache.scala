package slyce.parse.`macro`

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.collection.mutable
import scala.quoted.*
import scala.reflect.TypeTest

final class ExtractedTypeCache private (
    elementTypeRepr: TypeRepr,
    terminalTypeRepr: TypeRepr,
    nonTerminalTypeRepr: TypeRepr,
) {

  private val cache: mutable.Map[TypeRepr, ExtractedType] = mutable.Map.empty

  def getAllTypes: ArraySeq[ExtractedType] = ArraySeq.from(cache.values)

  def get(parentPos: Position)(tpe: TypeRepr)(using Quotes): ExtractedType =
    cache.get(tpe) match {
      case Some(value) => value
      case None        =>
        val extracted: ExtractedType = ExtractedType.doExtract(parentPos, this)(tpe)
        cache.update(tpe, extracted)
        extracted.initialize(parentPos, this)
        extracted
    }

  def getNarrowed[T <: ExtractedType: TypeTag as tag](parentPos: Position)(tpe: TypeRepr)(using TypeTest[ExtractedType, T], Quotes): T =
    get(parentPos)(tpe) match
      case t: T => t
      case res  => report.errorAndAbort(s"Type not allowed here [expected: $tag] [actual: ${TypeTag.fromClass(res.getClass)}]", parentPos)

  def elementType(pos: Position)(tpe: TypeRepr)(using Quotes): ElementType = {
    val isElementType: Boolean = tpe <:< elementTypeRepr
    val isTerminalType: Boolean = tpe <:< terminalTypeRepr
    val isNonTerminalType: Boolean = tpe <:< nonTerminalTypeRepr

    if !isElementType then report.errorAndAbort(s"Type ${tpe.showAnsiCode} does not extend Element", pos)

    (isTerminalType, isNonTerminalType) match {
      case (true, false)  => ElementType.Terminal
      case (false, true)  => ElementType.NonTerminal
      case (false, false) => ElementType.Element
      case (true, true)   => report.errorAndAbort(s"(Not possible?) Type ${tpe.showAnsiCode} extends both Terminal and NonTerminal", pos)
    }
  }

}
object ExtractedTypeCache {

  def empty(using Quotes): ExtractedTypeCache =
    new ExtractedTypeCache(
      elementTypeRepr = TypeRepr.of[slyce.core.Element],
      terminalTypeRepr = TypeRepr.of[slyce.core.Terminal],
      nonTerminalTypeRepr = TypeRepr.of[slyce.core.NonTerminal],
    )

}
