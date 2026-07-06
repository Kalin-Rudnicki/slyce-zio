package slyce.generate

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import scala.reflect.ClassTag

import slyce.parse.*

private[slyce] object DeriveParser {

  def derivedImpl[A: Type](
      maxLookAheadExpr: Expr[Int],
  )(using Quotes): Expr[Parser[A]] = {
    val maxLookAhead: Int =
      maxLookAheadExpr.evalOption.getOrElse { report.errorAndAbort("Requires int constant", maxLookAheadExpr) }

    val cache: ExtractedTypeCache = ExtractedTypeCache.empty
    val root: ExtractedType = cache.getOrCreate(Position.ofMacroExpansion)(TypeRepr.of[A])

    val customs: ArraySeq[ExtractedType.Custom] = cache.getAllTypes.collect { case et: ExtractedType.Custom => et }.sortBy(_.gen.typeRepr.showCode)

    val productTerminals: ArraySeq[ExtractedType.ProductTerminal] = customs.collect { case et: ExtractedType.ProductTerminal => et }
    val productNonTerminals: ArraySeq[ExtractedType.ProductNonTerminal] = customs.collect { case et: ExtractedType.ProductNonTerminal => et }
    val sumTerminals: ArraySeq[ExtractedType.SumTerminal] = customs.collect { case et: ExtractedType.SumTerminal => et }
    val sumNonTerminals: ArraySeq[ExtractedType.SumNonTerminal] = customs.collect { case et: ExtractedType.SumNonTerminal => et }
    val sumElements: ArraySeq[ExtractedType.SumElement] = customs.collect { case et: ExtractedType.SumElement => et }

    def section[T <: ExtractedType.Custom: {ClassTag as ct, TypeTag as tt}]: Text = {
      val elems: ArraySeq[T] = customs.collect { case ct(v) => v }
      str"\n \n=====| ${str"${tt.prefixObject}".cyanFg} (${str"${elems.length.toString}".magentaFg}) |=====\n " ++
        Text.foreach(elems) { elem => str"\n${elem.renderRoot}" } ++
        str"\n "
    }

    val shown: Text =
      section[ExtractedType.ProductTerminal] ++
        section[ExtractedType.ProductNonTerminal] ++
        section[ExtractedType.SumTerminal] ++
        section[ExtractedType.SumNonTerminal] ++
        section[ExtractedType.SumElement]

    report.errorAndAbort(shown.toString)

    '{ ??? } // FIX-PRE-MERGE (KR) :
  }

}
