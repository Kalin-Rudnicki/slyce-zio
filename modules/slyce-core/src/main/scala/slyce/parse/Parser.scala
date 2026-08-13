package slyce.parse

import slyce.core.*
import slyce.generate.DeriveParser

trait Parser[A] {
  def parse(source: Source): Either[ParseError, A]
}
object Parser {

  /** @param maxLookAhead
    *   try `1`, if that doesnt work, try `2`, if that doesnt work, your grammar probably needs to be tweaked
    */
  inline def derived[A](inline maxLookAhead: Int): Parser[A] = ${ DeriveParser.derivedImpl[A]('maxLookAhead) }

}
