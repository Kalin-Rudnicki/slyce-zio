package slyce.generate.grammar

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import klib.utils.IndentedString
import scala.annotation.tailrec

import slyce.core.*
import slyce.generate.*

final case class ParsingTable private (
    // TODO (KR) :
)
object ParsingTable {

  object fromExpandedGrammar {

    /*
       What do we want here?
       Effectively something along the lines of a List[State].
       What is a State?
       Map[Input, Action].
     */

    def apply(expandedGrammar: ExpandedGrammar): Validated[ParsingTable] = {
      // TODO (KR) :
      Validated.???
    }

  }
}
