package slyce.generate

import oxygen.quoted.*
import scala.quoted.*

import slyce.parse.*

private[slyce] object DeriveParser {

  def derivedImpl[A: Type](
      maxLookAheadExpr: Expr[Int],
  )(using Quotes): Expr[Parser[A]] = {
    val maxLookAhead: Int =
      maxLookAheadExpr.evalOption.getOrElse { report.errorAndAbort("Requires int constant", maxLookAheadExpr) }

    val cache: ExtractedTypeCache = ExtractedTypeCache.empty
    val root: ExtractedType = cache.getOrCreate(Position.ofMacroExpansion)(TypeRepr.of[A])

    val sorted = cache.getAllTypes.collect { case et: ExtractedType.Custom => et }.sortBy(_.typeRepr.showCode)

    val tmpOut = sorted.map(_.renderRoot).mkString(" \n", "\n\n\n", "\n ")

    report.errorAndAbort(tmpOut)

    '{ ??? } // FIX-PRE-MERGE (KR) :
  }

}
