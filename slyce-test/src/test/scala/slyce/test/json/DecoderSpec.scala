package slyce.test.json

import cats.syntax.option.*
import harness.test.*
import zio.test.*
import zio.test.Assertion.*

import slyce.test.json.Types.*

object DecoderSpec extends DefaultHarnessSpec {

  private def passingTest[T: JsonDecoder](text: String)(exp: T): TestSpec =
    test(text) {
      assert(Json.decode[T](text))(isRight(equalTo(exp)))
    }

  override def spec: TestSpec =
    suite("DecoderSpec")(
      suite("passes")(
        passingTest[Person]("""{ "firstName": "F", "lastName": "L", "age": 3, "friends": [] }""")(Person("F", "L", 3.some, Nil)),
        passingTest[Person]("""{ "firstName": "F", "lastName": "L", "age": null, "friends": [] }""")(Person("F", "L", None, Nil)),
        passingTest[Person]("""{ "firstName": "F", "lastName": "L", "friends": [] }""")(Person("F", "L", None, Nil)),
        passingTest[Person]("""{ "firstName": "F", "lastName": "L", "friends": [ { "name": "N" }, {}, null ] }""")(Person("F", "L", None, Nil)),
      ),
    )

}
