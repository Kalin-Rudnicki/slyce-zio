package slyce.generate

import cats.Functor
import cats.syntax.functor.*
import cats.syntax.option.*

final class Pointer[T] private (private var _value: Option[T]) {

  def value: T =
    _value match {
      case Some(value) => value
      case None        => throw RuntimeException("Improper use of Pointer")
    }

  override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case that: Pointer[_] => this.value == that.value
      case _                => false
    }

  override def hashCode(): Int =
    value.hashCode

}
object Pointer {

  def apply[T](t: T): Pointer[T] =
    new Pointer[T](t.some)

  def withSelf[T](f: Pointer[T] => Pointer[T]): Pointer[T] = {
    val self = new Pointer[T](None)
    val res = f(self)

    self._value = res._value
    self
  }

  def withSelfWrapped[T, F[_]: Functor](f: Pointer[T] => F[Pointer[T]]): F[Pointer[T]] = {
    val self = new Pointer[T](None)
    val res = f(self)

    res.map { t =>
      self._value = t._value
      self
    }
  }

}
