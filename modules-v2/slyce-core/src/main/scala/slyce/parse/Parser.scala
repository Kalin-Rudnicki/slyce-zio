package slyce.parse

import slyce.core.*
import slyce.parse.`macro`.DeriveParser

trait Parser[A] {
  def parse(source: Source): Either[ParseError, A]
}
object Parser {

  inline def derived[A]: Parser[A] = ${ DeriveParser.derivedImpl[A] }

}
