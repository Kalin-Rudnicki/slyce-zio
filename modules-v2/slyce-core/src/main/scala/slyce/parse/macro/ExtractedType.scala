package slyce.parse.`macro`

import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.quoted.TypeType.Case
import scala.quoted.*

import slyce.core.builtIn.*

private[`macro`] sealed trait ExtractedType {
  val typeRepr: TypeRepr
}
private[`macro`] object ExtractedType {

  sealed trait Custom extends ExtractedType
  sealed trait TerminalLike extends ExtractedType.Custom
  sealed trait NonTerminalLike extends ExtractedType.Custom
  sealed trait SumLike extends ExtractedType.Custom
  sealed trait ProductLike extends ExtractedType.Custom

  final case class ProductTerminal(
      typeRepr: TypeRepr, // TODO (KR) : derive?
  ) extends ExtractedType.ProductLike,
        ExtractedType.TerminalLike

  final case class ProductNonTerminal(
      typeRepr: TypeRepr, // TODO (KR) : derive?
  ) extends ExtractedType.ProductLike,
        ExtractedType.NonTerminalLike

  final case class SumTerminal(
      typeRepr: TypeRepr, // TODO (KR) : derive?
  ) extends ExtractedType.SumLike,
        ExtractedType.TerminalLike

  final case class SumNonTerminal(
      typeRepr: TypeRepr, // TODO (KR) : derive?
  ) extends ExtractedType.SumLike,
        ExtractedType.NonTerminalLike

  final case class SumElement(
      typeRepr: TypeRepr, // TODO (KR) : derive?
  ) extends ExtractedType.SumLike

  ///////  ///////////////////////////////////////////////////////////////

  sealed trait BuiltIn extends ExtractedType

  final case class IgnoreBuiltIn(typeRepr: TypeRepr) extends BuiltIn

  final case class UnionBuiltIn(
      typeRepr: TypeRepr,
      cases: NonEmptyList[Box[ExtractedType.Custom]],
  ) extends ExtractedType.BuiltIn

  final case class ElementOptionBuiltIn(
      typeRepr: TypeRepr,
      elem: Box[ExtractedType],
  ) extends ExtractedType.BuiltIn

  final case class ElementListBuiltIn(
      typeRepr: TypeRepr,
      elem: Box[ExtractedType],
  ) extends ExtractedType.BuiltIn

  final case class NonEmptyElementListBuiltIn(
      typeRepr: TypeRepr,
      elem: Box[ExtractedType],
  ) extends ExtractedType.BuiltIn

  final case class VElementListBuiltIn(
      typeRepr: TypeRepr,
      elem: Box[ExtractedType],
      before: Box[ExtractedType],
      between: Box[ExtractedType],
      after: Box[ExtractedType],
  ) extends ExtractedType.BuiltIn

  final case class NonEmptyVElementListBuiltIn(
      typeRepr: TypeRepr,
      elem: Box[ExtractedType],
      before: Box[ExtractedType],
      between: Box[ExtractedType],
      after: Box[ExtractedType],
  ) extends ExtractedType.BuiltIn

  // TODO (KR) : list/nonEmptyList/option/either/union/assoc

  ///////  ///////////////////////////////////////////////////////////////

  private def doExtractSealed(pos: Position)(cache: ExtractedTypeCache, returnBox: Box[ExtractedType])(tpe: TypeRepr)(using Quotes): Unit = {
    type T
    given Type[T] = tpe.asTypeOf
    val gen: SumGeneric[T] = SumGeneric.of[T]
    report.errorAndAbort(s"todo : doExtractSealed (${tpe.showAnsiCode})", gen.pos)
  }

  private def doExtractCaseClass(pos: Position)(cache: ExtractedTypeCache, returnBox: Box[ExtractedType])(tpe: TypeRepr)(using Quotes): Unit = {
    type T
    given Type[T] = tpe.asTypeOf
    val gen: ProductGeneric.CaseClassGeneric[T] = ProductGeneric.CaseClassGeneric.of[T]
    report.errorAndAbort(s"todo : doExtractCaseClass (${tpe.showAnsiCode})", gen.pos)
  }

  // TODO (KR) : include a path `Program.x.Thing.y.ThisType`
  private def doExtract(pos: Position)(cache: ExtractedTypeCache, returnBox: Box[ExtractedType])(tpe: TypeRepr)(using Quotes): Unit = {
    def fail(msg: String): Nothing = report.errorAndAbort(msg, pos)

    val underlyingType: TypeRepr = tpe.dealiasKeepOpaques

    // this is some non-fp bullshit...
    // doing the extra merry-go-round so that the box is technically populated
    (underlyingType, underlyingType.asType) match {

      /////// BuiltIn ///////////////////////////////////////////////////////////////
      case (underlyingType: OrType, _) =>
        val orChildren: NonEmptyList[TypeRepr] = NonEmptyList.unsafeFromList(underlyingType.orChildren.toList)
        val childPairs =
          orChildren.map { cTpe =>
            val (box, alreadyExists) = cache.get(cTpe)
            (box, alreadyExists, cTpe)
          }
        val res: UnionBuiltIn = UnionBuiltIn(typeRepr = underlyingType, cases = childPairs.map(_._1.narrow[ExtractedType.Custom]))
        returnBox.set(res)
        childPairs.toList.foreach {
          // TODO (KR) : create a `CheckedSet` box subtype which will ensure the type being put into this box meets the subtype, include error reason
          case (box, false, cTpe) => doExtract(pos)(cache, box)(cTpe)
          case (_, true, _)       => ()
        }
      case (_, '[ElementOption[elem]]) =>
        val elemType = TypeRepr.of[elem]
        val (elemBox, elemExists) = cache.get(elemType)
        val res: ElementOptionBuiltIn = ElementOptionBuiltIn(underlyingType, elemBox)
        returnBox.set(res)
        if !elemExists then doExtract(pos)(cache, elemBox)(elemType)

      /////// Explicitly Reject ///////////////////////////////////////////////////////////////
      case (_: AndType, _)         => fail(s"AndType not supported : ${underlyingType.showAnsiCode}")
      case (_, '[Option[a]])       => fail("Use ElementOption instead of Option (slyce.core.builtIn.*)")
      case (_, '[List[a]])         => fail("Use ElementList/VElementList instead of List (slyce.core.builtIn.*)")
      case (_, '[NonEmptyList[a]]) => fail("Use NonEmptyElementList/NonEmptyVElementList instead of NonEmptyList (slyce.core.builtIn.*)")

      /////// Custom ///////////////////////////////////////////////////////////////
      case _ =>
        underlyingType.typeType.option match {
          case Some(value) =>
            value match {
              case _: TypeType.Sealed => doExtractSealed(pos)(cache, returnBox)(underlyingType)
              case _: Case.Class      => doExtractCaseClass(pos)(cache, returnBox)(underlyingType)
              case _: Case.Object     => fail(s"Unable to derive parser for case object : ${underlyingType.showAnsiCode}")
            }
          case None => fail(s"Unknown type : ${underlyingType.showAnsiCode}")
        }

    }
  }

  def extractRoot(tpe: TypeRepr)(using Quotes): Box[ExtractedType] = {
    val cache = ExtractedTypeCache.empty
    val (box, _) = cache.get(tpe)
    doExtract(Position.ofMacroExpansion)(cache, box)(tpe)
    box
  }

}
