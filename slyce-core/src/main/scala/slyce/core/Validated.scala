package slyce.core

import cats.data.EitherNel
import cats.syntax.either.*
import cats.syntax.parallel.*
import harness.core.*
import harness.zio.*
import zio.*

import slyce.core.*

type Validated[T] = EitherNel[Marked[String], T]
object Validated {

  def withValidations[T](validations: Validated[Any]*)(ifValid: => Validated[T]): Validated[T] =
    validations.toList.parTraverse(identity).flatMap { _ => ifValid }

  // TODO (KR) : move this into a different project to allow for only harness-core
  def toHTask[A](validated: Validated[A]): HTask[A] =
    validated match {
      case Left(errors) => ZIO.fail(HError.UserError(Source.markAll(errors.toList)))
      case Right(value) => ZIO.succeed(value)
    }

  val ??? : Validated[Nothing] = Marked("???", Span.Unknown).leftNel

}
