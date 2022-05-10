package slyce.generate

import cats.data.EitherNel
import cats.syntax.parallel.*

import slyce.core.Marked

type Validated[T] = EitherNel[Marked[String], T]
object Validated {

  def withValidations[T](validations: Validated[Any]*)(ifValid: => Validated[T]): Validated[T] =
    validations.toList.parTraverse(identity).flatMap { _ => ifValid }

}
