package slyce.test.json

import harness.core.StringEncoder

trait JsonEncoder[-T] { self =>

  def encode(t: T): Json

  final def cmap[T2](f: T2 => T): JsonEncoder[T2] =
    t => self.encode(f(t))

}
object JsonEncoder {

  inline def apply[T](implicit je: JsonEncoder[T]): JsonEncoder[T] = je

  implicit val json: JsonEncoder[Json] = identity(_)

  implicit val unit: JsonEncoder[Unit] = _ => Json.JsonNull
  implicit val string: JsonEncoder[String] = Json.string(_)
  implicit val boolean: JsonEncoder[Boolean] = Json.boolean(_)
  implicit val int: JsonEncoder[Int] = Json.int(_)
  implicit val bigDecimal: JsonEncoder[BigDecimal] = Json.double(_)
  implicit val double: JsonEncoder[Double] = Json.double(_)
  implicit val float: JsonEncoder[Float] = Json.double(_)
  implicit def array[T: JsonEncoder]: JsonEncoder[Seq[T]] = value => Json.JsonArray(value.map(JsonEncoder[T].encode).toList)
  implicit def map[T: JsonEncoder]: JsonEncoder[Map[String, T]] = value => Json.JsonObject(value.map { (k, v) => (k, JsonEncoder[T].encode(v)) })

  implicit def optional[T: JsonEncoder]: JsonEncoder[Option[T]] = {
    case Some(value) => JsonEncoder[T].encode(value)
    case None        => Json.JsonNull
  }
  
  implicit def fromStringEncoder[T: StringEncoder]: JsonEncoder[T] = JsonEncoder.string.cmap(StringEncoder[T].encode)
  
}
