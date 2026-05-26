package slyce.core

import cats.data.EitherNel
import cats.syntax.either.*
import cats.syntax.parallel.*

import slyce.core.*

type Validated[T] = EitherNel[Marked[String], T]
object Validated {

  def withValidations[T](validations: Validated[Any]*)(ifValid: => Validated[T]): Validated[T] =
    validations.toList.parTraverse(identity).flatMap { _ => ifValid }

  val ??? : Validated[Nothing] = Marked("???", Span.Unknown).leftNel

}
