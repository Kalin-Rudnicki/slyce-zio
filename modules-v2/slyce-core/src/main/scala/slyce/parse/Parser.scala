package slyce.parse

import scala.quoted.*

import slyce.core.*

trait Parser[A] {
  def parse(source: Source): Either[ParseError, A]
}
object Parser {

  private def derivedImpl[A: Type](using Quotes): Expr[Parser[A]] =
    ??? // FIX-PRE-MERGE (KR) :

  inline def derived[A]: Parser[A] = ${ derivedImpl[A] }

}
