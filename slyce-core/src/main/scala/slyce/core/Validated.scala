package slyce.core

import cats.data.EitherNel
import cats.syntax.either.*
import cats.syntax.parallel.*
import klib.utils.*
import zio.*

import slyce.core.*

type Validated[T] = EitherNel[Marked[String], T]
object Validated {

  def withValidations[T](validations: Validated[Any]*)(ifValid: => Validated[T]): Validated[T] =
    validations.toList.parTraverse(identity).flatMap { _ => ifValid }

  def toKTask[A](validated: Validated[A]): KTask[A] =
    validated match {
      case Left(errors) => ZIO.failNEL(KError.UserError(Source.markAll(errors.toList)))
      case Right(value) => ZIO.succeed(value)
    }

  val ??? : Validated[Nothing] = Marked("???", Span.Unknown).leftNel

}
