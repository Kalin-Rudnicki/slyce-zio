package slyce.generate.error

import cats.data.NonEmptyList
import harness.core.*

import slyce.core.*

sealed trait GenerateError extends Throwable {

  override final def getMessage: String = this match {
    case GenerateError.FailedValidation(errors) => Source.markAll(errors.toList)
    case GenerateError.InvalidInput(message)    => message
    case GenerateError.Unexpected(error)        => s"Unexpected error: ${error.safeGetMessage}"
  }

}
object GenerateError {

  final case class FailedValidation(errors: NonEmptyList[Marked[String]]) extends GenerateError
  final case class InvalidInput(message: String) extends GenerateError
  final case class Unexpected(error: Throwable) extends GenerateError

}
