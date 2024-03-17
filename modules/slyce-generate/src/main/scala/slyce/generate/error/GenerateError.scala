package slyce.generate.error

import cats.data.NonEmptyList

import slyce.core.Marked

sealed trait GenerateError extends Throwable
object GenerateError {

  final case class FailedValidation(errors: NonEmptyList[Marked[String]]) extends GenerateError
  final case class InvalidInput(message: String) extends GenerateError
  final case class Unexpected(error: Throwable) extends GenerateError

}
