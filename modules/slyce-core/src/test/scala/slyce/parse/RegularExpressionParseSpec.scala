package slyce.parse

import oxygen.predef.test.*

import slyce.core.*

object RegularExpressionParseSpec extends OxygenSpecDefault {

  private def makeTest(regex: scala.util.matching.Regex)(exp: RegularExpression): TestSpec =
    test(regex.regex) {
      val actual = RegularExpression.parse(Source(regex.regex, None))
      assert(actual)(isRight(equalTo(exp)))
    }

  override def testSpec: TestSpec =
    (suite("RegularExpressionParseSpec")(
      makeTest("abc".r)(
        RegularExpression.Sequence("abc"),
      ),
      makeTest("abc|def|ghi|jkl".r)(
        RegularExpression.Group(
          RegularExpression.Sequence("abc"),
          RegularExpression.Sequence("def"),
          RegularExpression.Sequence("ghi"),
          RegularExpression.Sequence("jkl"),
        ),
      ),
      makeTest("(abc|def)|(ghi|jkl)".r)(
        RegularExpression.Group(
          RegularExpression.Group(
            RegularExpression.Sequence("abc"),
            RegularExpression.Sequence("def"),
          ),
          RegularExpression.Group(
            RegularExpression.Sequence("ghi"),
            RegularExpression.Sequence("jkl"),
          ),
        ),
      ),
      makeTest("(abc|def)ghi|jkl".r)(
        RegularExpression.Group(
          RegularExpression.Sequence(
            RegularExpression.Group(
              RegularExpression.Sequence("abc"),
              RegularExpression.Sequence("def"),
            ),
            RegularExpression.Sequence("ghi"),
          ),
          RegularExpression.Sequence("jkl"),
        ),
      ),
      makeTest("abc|def(ghi|jkl)".r)(
        RegularExpression.Group(
          RegularExpression.Sequence("abc"),
          RegularExpression.Sequence(
            RegularExpression.Sequence("def"),
            RegularExpression.Group(
              RegularExpression.Sequence("ghi"),
              RegularExpression.Sequence("jkl"),
            ),
          ),
        ),
      ),
      makeTest("ab+".r)(
        RegularExpression.Sequence(
          RegularExpression.CharClass.inclusive('a'),
          RegularExpression.CharClass.inclusive('b').atLeastOnce,
        ),
      ),
      makeTest("[ab]+".r)(
        RegularExpression.Sequence(
          RegularExpression.CharClass.inclusive('a', 'b').atLeastOnce,
        ),
      ),
      makeTest("[^ab]c+".r)(
        RegularExpression.Sequence(
          RegularExpression.CharClass.exclusive('a', 'b'),
          RegularExpression.CharClass.inclusive('c').atLeastOnce,
        ),
      ),
      makeTest("[A-Za-z_][A-Za-z_\\d]+".r)(
        RegularExpression.Sequence(
          RegularExpression.CharClass.inclusiveRange('A', 'Z') |
            RegularExpression.CharClass.inclusiveRange('a', 'z') |
            RegularExpression.CharClass.inclusive('_'),
          (
            RegularExpression.CharClass.inclusiveRange('A', 'Z') |
              RegularExpression.CharClass.inclusiveRange('a', 'z') |
              RegularExpression.CharClass.inclusive('_') |
              RegularExpression.CharClass.inclusiveRange('0', '9')
          ).atLeastOnce,
        ),
      ),
      makeTest("-?\\d+".r)(
        RegularExpression.Sequence(
          RegularExpression.CharClass.inclusive('-').optional,
          RegularExpression.CharClass.inclusiveRange('0', '9').atLeastOnce,
        ),
      ),
      makeTest("-?\\d+\\.\\d+".r)(
        RegularExpression.Sequence(
          RegularExpression.CharClass.inclusive('-').optional,
          RegularExpression.CharClass.inclusiveRange('0', '9').atLeastOnce,
          RegularExpression.CharClass.inclusive('.'),
          RegularExpression.CharClass.inclusiveRange('0', '9').atLeastOnce,
        ),
      ),
    )) @@ TestAspect.ignore // calculator e2e isolation

}
