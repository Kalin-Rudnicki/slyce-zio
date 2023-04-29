package slyce.test.json

import harness.test.*
import zio.test.*
import zio.test.Assertion.*

object ParseSpec extends DefaultHarnessSpec {

  private def passingTest(text: String)(exp: Json): TestSpec =
    test(text) {
      assert(Json.parse(text))(isRight(equalTo(exp)))
    }

  override def spec: TestSpec =
    suite("ParseSpec")(
      suite("passes")(
        passingTest("null")(Json.`null`),
        passingTest("true")(Json.boolean(true)),
        passingTest("false")(Json.boolean(false)),
        passingTest("1")(Json.int(1)),
        passingTest("1.2")(Json.double(1.2)),
        passingTest("[]")(Json.array()),
        passingTest("{}")(Json.obj()),
        passingTest("""[ null, [], {}, true, "oops" ]""")(
          Json.array(Json.`null`, Json.array(), Json.obj(), Json.boolean(true), Json.string("oops")),
        ),
        passingTest("""{ "a": null, "b": [], "c": {}, "d": true, "e": "oops" }""")(
          Json.obj("a" -> Json.`null`, "b" -> Json.array(), "c" -> Json.obj(), "d" -> Json.boolean(true), "e" -> Json.string("oops")),
        ),
      ),
    )

}
