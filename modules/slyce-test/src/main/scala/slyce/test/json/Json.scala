package slyce.test.json

import cats.data.EitherNel
import cats.syntax.either.*
import cats.syntax.option.*

import slyce.core.Source

sealed trait Json
object Json {

  // =====| ADT |=====

  case object JsonNull extends Json
  final case class JsonBoolean(value: Boolean) extends Json
  final case class JsonString(value: String) extends Json
  final case class JsonInt(value: Int) extends Json // TODO (KR) : BigInt
  final case class JsonDouble(value: BigDecimal) extends Json
  final case class JsonArray(value: List[Json]) extends Json
  final case class JsonObject(value: Map[String, Json]) extends Json

  // =====| Parsing |=====

  def parse(source: Source): Either[String, Json] =
    JsonParser.parse(source) match {
      case Right(json)  => Conversion.convert(json).asRight
      case Left(errors) => source.mark(errors.toList).asLeft
    }
  def parse(text: String, sourceName: String): Either[String, Json] =
    Json.parse(Source(text, sourceName.some))
  def parse(text: String): Either[String, Json] =
    Json.parse(Source(text, None))

  def decode[T: JsonDecoder](source: Source): EitherNel[String, T] =
    Json.parse(source) match {
      case Right(json) => JsonDecoder[T].decodeAccumulating(json).leftMap(_.map(_.toString))
      case Left(error) => error.leftNel
    }
  def decode[T: JsonDecoder](text: String, sourceName: String): EitherNel[String, T] =
    Json.decode(Source(text, sourceName.some))
  def decode[T: JsonDecoder](text: String): EitherNel[String, T] =
    Json.decode(Source(text, None))

  // =====|  |=====

  val `null`: Json = JsonNull
  def boolean(value: Boolean): Json = JsonBoolean(value)
  def string(value: String): Json = JsonString(value)
  def int(value: Int): Json = JsonInt(value)
  def double(value: Double): Json = JsonDouble(BigDecimal(value))
  def double(value: BigDecimal): Json = JsonDouble(value)
  def array(values: Json*): Json = JsonArray(values.toList)
  def obj(pairs: (String, Json)*): Json = JsonObject(pairs.toMap)

}
