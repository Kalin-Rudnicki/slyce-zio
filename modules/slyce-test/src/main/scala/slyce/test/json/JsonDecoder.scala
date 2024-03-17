package slyce.test.json

import cats.data.EitherNel
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import harness.core.StringDecoder

trait JsonDecoder[+T] { self =>

  def decodeAccumulating(json: Json): EitherNel[JsonDecoder.Fail, T]
  def decode(json: Json): Either[JsonDecoder.Fail, T] = self.decodeAccumulating(json).leftMap(_.head)

  def produce: Option[T] = None

  final def map[T2](f: T => T2): JsonDecoder[T2] =
    self.decodeAccumulating(_).map(f)
  final def emap[T2](f: T => EitherNel[JsonDecoder.Fail, T2]): JsonDecoder[T2] =
    self.decodeAccumulating(_).flatMap(f)

  final def flatMap[T2](f: T => JsonDecoder[T2]): JsonDecoder[T2] =
    json => self.decodeAccumulating(json).flatMap(f(_).decodeAccumulating(json))

  final def <*>[T2](other: JsonDecoder[T2])(implicit zip: zio.Zippable[T, T2]): JsonDecoder[zip.Out] =
    json =>
      (self.decodeAccumulating(json), other.decodeAccumulating(json)) match {
        case (Right(v1), Right(v2))         => zip.zip(v1, v2).asRight
        case (Left(errors), Right(_))       => errors.asLeft
        case (Right(_), Left(errors))       => errors.asLeft
        case (Left(errors1), Left(errors2)) => (errors1 ::: errors2).asLeft
      }

}
object JsonDecoder {

  inline def apply[T](implicit je: JsonDecoder[T]): JsonDecoder[T] = je

  sealed trait Fail {

    private final def msg: String =
      this match {
        case Fail.RootCause(cause)            => s" : $cause"
        case Fail.DownFieldArray(index, fail) => s"[$index]${fail.msg}"
        case Fail.DownFieldObject(key, fail)  => s".$key${fail.msg}"
      }

    override final def toString: String = s"_root_$msg"

  }
  object Fail {
    final case class RootCause(cause: String) extends JsonDecoder.Fail
    final case class DownFieldArray(index: Int, fail: JsonDecoder.Fail) extends JsonDecoder.Fail
    final case class DownFieldObject(key: String, fail: JsonDecoder.Fail) extends JsonDecoder.Fail
  }

  inline private def fromPartial[T](expType: String)(inline pf: PartialFunction[Json, T]): JsonDecoder[T] =
    json =>
      pf.lift(json) match {
        case Some(value) => value.asRight
        case None        => JsonDecoder.Fail.RootCause(s"expected type '$expType'").leftNel
      }

  implicit val json: JsonDecoder[Json] = _.asRight
  implicit val jsonObject: JsonDecoder[Map[String, Json]] = JsonDecoder.fromPartial("object") { case Json.JsonObject(value) => value }

  implicit val unit: JsonDecoder[Unit] = JsonDecoder.fromPartial("null") { case Json.JsonNull => () }
  implicit val string: JsonDecoder[String] = JsonDecoder.fromPartial("string") { case Json.JsonString(value) => value }
  implicit val boolean: JsonDecoder[Boolean] = JsonDecoder.fromPartial("boolean") { case Json.JsonBoolean(value) => value }
  implicit val int: JsonDecoder[Int] = JsonDecoder.fromPartial("int") { case Json.JsonInt(value) => value }
  implicit val bigDecimal: JsonDecoder[BigDecimal] = JsonDecoder.fromPartial("double") {
    case Json.JsonDouble(value) => value
    case Json.JsonInt(value)    => BigDecimal(value)
  }
  implicit val float: JsonDecoder[Float] = JsonDecoder.bigDecimal.map(_.toFloat)
  implicit val double: JsonDecoder[Double] = JsonDecoder.bigDecimal.map(_.toDouble)
  implicit def array[T: JsonDecoder]: JsonDecoder[List[T]] =
    JsonDecoder.fromPartial("array") { case Json.JsonArray(value) => value }.emap {
      _.zipWithIndex.parTraverse { (json, idx) =>
        JsonDecoder[T].decodeAccumulating(json).leftMap(_.map(JsonDecoder.Fail.DownFieldArray(idx, _)))
      }
    }
  implicit def map[T: JsonDecoder]: JsonDecoder[Map[String, T]] =
    JsonDecoder.jsonObject.emap {
      _.toList
        .parTraverse { (key, json) =>
          JsonDecoder[T].decodeAccumulating(json) match {
            case Right(value) => (key, value).asRight
            case Left(errors) => errors.map(JsonDecoder.Fail.DownFieldObject(key, _)).asLeft
          }
        }
        .map(_.toMap)
    }

  implicit def optional[T: JsonDecoder]: JsonDecoder[Option[T]] =
    new JsonDecoder[Option[T]] {
      override def decodeAccumulating(json: Json): EitherNel[Fail, Option[T]] =
        json match {
          case Json.JsonNull => None.asRight
          case json          => JsonDecoder[T].decodeAccumulating(json).map(_.some)
        }
      override def produce: Option[Option[T]] = None.some
    }

  implicit def fromStringDecoder[T: StringDecoder]: JsonDecoder[T] =
    JsonDecoder.string.emap(StringDecoder[T].decodeAccumulating(_).leftMap(_.map(JsonDecoder.Fail.RootCause.apply)))

  def forKey[T: JsonDecoder](key: String): JsonDecoder[T] =
    JsonDecoder.jsonObject.emap { map =>
      (map.get(key) match {
        case Some(json) => JsonDecoder[T].decodeAccumulating(json)
        case None =>
          JsonDecoder[T].produce match {
            case Some(value) => value.asRight
            case None        => JsonDecoder.Fail.RootCause("missing required key").leftNel
          }
      }).leftMap(_.map(JsonDecoder.Fail.DownFieldObject(key, _)))
    }

}
