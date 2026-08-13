package slyce

import oxygen.predef.test.*

import slyce.core.*
import slyce.core.builtIn.*
import slyce.parse.*

/** All test helpers should accept `Trace` and `SourceLocation` (as `using` params) so call-site locations flow into zio-test assertions / failure reporting.
  */
object TestUtils {

  def source(text: String): Source = Source(text, None)

  def span(s: Source, start: Int, end: Int): Span.Range =
    Span.Range(s, s.positions(start), s.positions(end))

  /** Span of the first occurrence of `piece` in `s.text` at or after `from`. */
  def spanOf(s: Source, piece: String, from: Int = 0): Span.Range = {
    val idx = s.text.indexOf(piece, from)
    require(idx >= 0, s"piece not found in source: ${piece.unesc}")
    span(s, idx, idx + piece.length)
  }

  def eofSpan(s: Source): Span.Range =
    span(s, s.length, s.length)

  def elementList[E <: Element](emptySpan: Span.Range)(elems: E*): ElementList[E] =
    elems.foldRight[ElementList[E]](ElementNil(emptySpan))(NonEmptyElementList(_, _))

  def eoSome[E <: Element](e: E): ElementOption[E] = ElementOption.Some(e)

  def eoNone(span: Span.Range): ElementOption[Nothing] = ElementOption.None(span)

  def parsesTo[A](parser: Parser[A], input: String)(expected: Source => A)(using Trace, SourceLocation): zio.test.Spec[Any, Nothing] =
    test(input) {
      val s = source(input)
      assert(parser.parse(s))(isRight(equalTo(expected(s))))
    }

  def failsToParse[A](parser: Parser[A], input: String)(using Trace, SourceLocation): zio.test.Spec[Any, Nothing] =
    test(input) {
      assert(parser.parse(source(input)))(isLeft)
    }

}
