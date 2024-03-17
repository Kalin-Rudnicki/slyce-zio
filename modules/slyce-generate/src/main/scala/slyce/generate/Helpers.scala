package slyce.generate

import cats.syntax.option.*
import harness.core.*
import harness.zio.*
import java.util.UUID
import scala.annotation.tailrec
import zio.*

import slyce.core.*
import slyce.generate.error.GenerateError

object Helpers {

  @tailrec
  def findAll[T](
      unseen: Set[T],
      seen: Set[T] = Set.empty[T],
  )(
      findF: T => Set[T],
  ): Set[T] = {
    val newSeen = seen | unseen
    val newUnseen = unseen.flatMap(findF) &~ newSeen

    if (newUnseen.nonEmpty)
      findAll(newUnseen, newSeen)(findF)
    else
      newSeen
  }

  trait ExactEquality {
    private final val eeUUID: UUID = UUID.randomUUID
    final override def hashCode(): Int = eeUUID.hashCode
    final override def equals(obj: Any): Boolean =
      obj.asInstanceOf[Matchable] match {
        case that: ExactEquality => this.eeUUID == that.eeUUID
        case _                   => false
      }
  }

  def validatedToHTask[A](validated: Validated[A]): IO[GenerateError.FailedValidation, A] =
    validated match {
      case Left(errors) => ZIO.fail(GenerateError.FailedValidation(errors))
      case Right(value) => ZIO.succeed(value)
    }

  def sourceFromFile(file: Path): Task[Source] =
    file.readString.map(Source(_, file.show.some))

}

extension [A](a: A) {

  def someWhen(f: A => Boolean): Option[A] =
    Option.when(f(a))(a)

}
