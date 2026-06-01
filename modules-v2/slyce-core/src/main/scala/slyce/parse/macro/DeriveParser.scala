package slyce.parse.`macro`

import oxygen.quoted.*
import scala.quoted.*

import slyce.parse.*

private[parse] object DeriveParser {

  def derivedImpl[A: Type](using Quotes): Expr[Parser[A]] = {
    val res: Box[ExtractedType] = ExtractedType.extractRoot(TypeRepr.of[A])
    report.errorAndAbort("todo...")
  }

}
