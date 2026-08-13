package slyce.core

import oxygen.predef.test.*

object SourceMarkSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("SourceMarkSpec")(
      test("mark underlines a range") {
        val src = Source("hello world", Some("t.ambr"))
        val span = Span.Range(src, src.positions(6), src.positions(11))
        val out = Source.mark(src, List(Marked("bad word", span)), Source.Config.Plain)
        assertTrue(
          out.contains("[t.ambr]:"),
          out.contains("hello world"),
          out.contains("bad word"),
          out.contains("^"),
        )
      },
      test("markAll groups by source") {
        val a = Source("aaa", Some("a"))
        val b = Source("bbb", Some("b"))
        val ma = Marked("err-a", Span.Range(a, a.positions(0), a.positions(1)))
        val mb = Marked("err-b", Span.Range(b, b.positions(1), b.positions(2)))
        val out = Source.markAll(List(ma, mb), Source.Config.Plain)
        assertTrue(out.contains("[a]:"), out.contains("[b]:"), out.contains("err-a"), out.contains("err-b"))
      },
    )

}
