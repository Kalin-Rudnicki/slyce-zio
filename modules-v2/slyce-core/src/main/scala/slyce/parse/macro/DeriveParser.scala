package slyce.parse.`macro`

import oxygen.quoted.*
import scala.quoted.*

import slyce.parse.*

private[parse] object DeriveParser {

  def derivedImpl[A: Type](using Quotes): Expr[Parser[A]] = {
    val cache: ExtractedTypeCache = ExtractedTypeCache.empty
    val root: ExtractedType = cache.get(Position.ofMacroExpansion)(TypeRepr.of[A])

    val sorted = cache.getAllTypes.collect { case et: ExtractedType.Custom => et }.sortBy(_.typeRepr.showCode)

    report.errorAndAbort(
      sorted.map(_.renderRoot).mkString(" \n", "\n\n\n", "\n "),
    )
  }

}
