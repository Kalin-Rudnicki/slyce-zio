package slyce

import oxygen.predef.test.*

/**
 * All test helpers should accept `Trace` and `SourceLocation` (as `using` params)
 * so call-site locations flow into zio-test assertions / failure reporting.
 */
object TestUtils {

  def placeholder(using Trace, SourceLocation): Spec[Any, Nothing] =
    test("placeholder") {
      assertCompletes
    }

}
