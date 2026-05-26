package slyce.test.json

import oxygen.predef.core.*

trait JsonEncoder[-T] { self =>

  def encode(t: T): Json

  final def cmap[T2](f: T2 => T): JsonEncoder[T2] =
    t => self.encode(f(t))

}
object JsonEncoder {

  inline def apply[T](using je: JsonEncoder[T]): JsonEncoder[T] = je

  given json: JsonEncoder[Json] = identity(_)

  given unit: JsonEncoder[Unit] = _ => Json.JsonNull
  given string: JsonEncoder[String] = Json.string(_)
  given boolean: JsonEncoder[Boolean] = Json.boolean(_)
  given int: JsonEncoder[Int] = Json.int(_)
  given bigDecimal: JsonEncoder[BigDecimal] = Json.double(_)
  given double: JsonEncoder[Double] = Json.double(_)
  given float: JsonEncoder[Float] = Json.double(_)
  given array: [T: JsonEncoder] => JsonEncoder[Seq[T]] = value => Json.JsonArray(value.map(JsonEncoder[T].encode).toList)
  given map: [T: JsonEncoder] => JsonEncoder[Map[String, T]] = value => Json.JsonObject(value.map { (k, v) => (k, JsonEncoder[T].encode(v)) })

  given optional: [T: JsonEncoder] => JsonEncoder[Option[T]] = {
    case Some(value) => JsonEncoder[T].encode(value)
    case None        => Json.JsonNull
  }

  given fromStringEncoder: [T: StringEncoder] => JsonEncoder[T] = JsonEncoder.string.cmap(StringEncoder[T].encode)

}
