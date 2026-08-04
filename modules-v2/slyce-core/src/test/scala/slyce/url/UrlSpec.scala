package slyce.url

import oxygen.predef.test.*

import slyce.TestUtils.*
import slyce.core.*
import slyce.core.builtIn.*
import slyce.url.model.*

object UrlSpec extends OxygenSpecDefault {

  private def domainHost(s: Source, hostText: String, from: Int = 0): DomainHost = {
    val hostStart = s.text.indexOf(hostText, from)
    require(hostStart >= 0, s"host not found: $hostText")
    val labels = hostText.split('.')
    var offset = hostStart
    val head = DomainLabel(labels.head, span(s, offset, offset + labels.head.length))
    offset += labels.head.length
    val dots =
      labels.tail.map { lab =>
        val dot = `.`(".", span(s, offset, offset + 1))
        offset += 1
        val label = DomainLabel(lab, span(s, offset, offset + lab.length))
        offset += lab.length
        DotDomainLabel(dot, label)
      }
    DomainHost(head, elementList[DotDomainLabel](span(s, offset, offset))(dots*))
  }

  private def ipv4Host(s: Source, text: String = "127.0.0.1", from: Int = 0): Ipv4Host = {
    val start = s.text.indexOf(text, from)
    require(start >= 0, s"ipv4 not found: $text")
    val parts = text.split('.')
    require(parts.length == 4)
    def oct(i: Int, at: Int): (Ipv4Octet, Int) = {
      val t = parts(i)
      (Ipv4Octet(t, span(s, at, at + t.length), t.toInt), at + t.length)
    }
    val (a, afterA) = oct(0, start)
    val d1 = `.`(".", span(s, afterA, afterA + 1))
    val (b, afterB) = oct(1, afterA + 1)
    val d2 = `.`(".", span(s, afterB, afterB + 1))
    val (c, afterC) = oct(2, afterB + 1)
    val d3 = `.`(".", span(s, afterC, afterC + 1))
    val (d, _) = oct(3, afterC + 1)
    Ipv4Host(a, d1, b, d2, c, d3, d)
  }

  private def afterHost(s: Source, hostText: String): Int =
    s.text.indexOf(hostText) + hostText.length

  private def bareUrl(s: Source, scheme: String, hostText: String): Url = {
    val end = afterHost(s, hostText)
    Url(
      scheme = Scheme(scheme, spanOf(s, scheme)),
      sep = `://`("://", spanOf(s, "://")),
      host = domainHost(s, hostText),
      port = eoNone(span(s, end, end)),
      path = eoNone(span(s, end, end)),
      query = eoNone(span(s, end, end)),
      fragment = eoNone(span(s, end, end)),
    )
  }

  override def testSpec: TestSpec =
    suite("UrlSpec")(
      suite("valid")(
        parsesTo(Url.parser, "https://example.com") { s =>
          bareUrl(s, "https", "example.com")
        },
        parsesTo(Url.parser, "http://localhost") { s =>
          bareUrl(s, "http", "localhost")
        },
        parsesTo(Url.parser, "https://example.com:8080") { s =>
          val hostEnd = afterHost(s, "example.com")
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoSome(
              Port(
                `:`(":", span(s, hostEnd, hostEnd + 1)),
                PortNum("8080", spanOf(s, "8080"), 8080),
              ),
            ),
            path = eoNone(eofSpan(s)),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com/a") { s =>
          val slash = s.text.indexOf('/', s.text.indexOf("://") + 3)
          val hostEnd = afterHost(s, "example.com")
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoNone(span(s, hostEnd, hostEnd)),
            path = eoSome(
              PathWithSegs(
                PathSeg(`/`("/", span(s, slash, slash + 1)), PathSegment("a", span(s, slash + 1, s.length))),
                eoNone(eofSpan(s)),
              ),
            ),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com/a/b") { s =>
          val slash0 = s.text.indexOf('/', s.text.indexOf("://") + 3)
          val slash1 = s.text.indexOf('/', slash0 + 1)
          val hostEnd = afterHost(s, "example.com")
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoNone(span(s, hostEnd, hostEnd)),
            path = eoSome(
              PathWithSegs(
                PathSeg(`/`("/", span(s, slash0, slash0 + 1)), PathSegment("a", span(s, slash0 + 1, slash1))),
                eoSome(
                  PathRest(
                    `/`("/", span(s, slash1, slash1 + 1)),
                    eoSome(PathRestSeg(PathSegment("b", span(s, slash1 + 1, s.length)), eoNone(eofSpan(s)))),
                  ),
                ),
              ),
            ),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com/") { s =>
          val slash = s.text.lastIndexOf('/')
          val hostEnd = afterHost(s, "example.com")
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoNone(span(s, hostEnd, hostEnd)),
            path = eoSome(RootOnly(`/`("/", span(s, slash, slash + 1)))),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com/a/") { s =>
          val slash0 = s.text.indexOf('/', s.text.indexOf("://") + 3)
          val slash1 = s.text.lastIndexOf('/')
          val hostEnd = afterHost(s, "example.com")
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoNone(span(s, hostEnd, hostEnd)),
            path = eoSome(
              PathWithSegs(
                PathSeg(`/`("/", span(s, slash0, slash0 + 1)), PathSegment("a", span(s, slash0 + 1, slash1))),
                eoSome(PathRest(`/`("/", span(s, slash1, slash1 + 1)), eoNone(eofSpan(s)))),
              ),
            ),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "http://127.0.0.1") { s =>
          val end = afterHost(s, "127.0.0.1")
          Url(
            scheme = Scheme("http", spanOf(s, "http")),
            sep = `://`("://", spanOf(s, "://")),
            host = ipv4Host(s, "127.0.0.1"),
            port = eoNone(span(s, end, end)),
            path = eoNone(span(s, end, end)),
            query = eoNone(span(s, end, end)),
            fragment = eoNone(span(s, end, end)),
          )
        },
        parsesTo(Url.parser, "http://192.168.0.1:8080/") { s =>
          val slash = s.text.lastIndexOf('/')
          Url(
            scheme = Scheme("http", spanOf(s, "http")),
            sep = `://`("://", spanOf(s, "://")),
            host = ipv4Host(s, "192.168.0.1"),
            port = eoSome(
              Port(
                `:`(":", spanOf(s, ":", from = s.text.indexOf("1:8080") + 1)),
                PortNum("8080", spanOf(s, "8080"), 8080),
              ),
            ),
            path = eoSome(RootOnly(`/`("/", span(s, slash, slash + 1)))),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com?x=1") { s =>
          val hostEnd = afterHost(s, "example.com")
          val qAt = s.text.indexOf('?')
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoNone(span(s, hostEnd, hostEnd)),
            path = eoNone(span(s, hostEnd, hostEnd)),
            query = eoSome(
              Query(
                QMark("?", span(s, qAt, qAt + 1)),
                QueryPair(
                  QueryKey("x", span(s, qAt + 1, qAt + 2)),
                  `=`("=", span(s, qAt + 2, qAt + 3)),
                  QueryValue("1", span(s, qAt + 3, qAt + 4)),
                ),
                elementList[AndQueryPair](eofSpan(s))(),
              ),
            ),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com?x=1&y=2") { s =>
          val hostEnd = afterHost(s, "example.com")
          val qAt = s.text.indexOf('?')
          val amp = s.text.indexOf('&')
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoNone(span(s, hostEnd, hostEnd)),
            path = eoNone(span(s, hostEnd, hostEnd)),
            query = eoSome(
              Query(
                QMark("?", span(s, qAt, qAt + 1)),
                QueryPair(
                  QueryKey("x", span(s, qAt + 1, qAt + 2)),
                  `=`("=", span(s, qAt + 2, qAt + 3)),
                  QueryValue("1", span(s, qAt + 3, qAt + 4)),
                ),
                elementList[AndQueryPair](eofSpan(s))(
                  AndQueryPair(
                    `&`("&", span(s, amp, amp + 1)),
                    QueryPair(
                      QueryKey("y", span(s, amp + 1, amp + 2)),
                      `=`("=", span(s, amp + 2, amp + 3)),
                      QueryValue("2", span(s, amp + 3, amp + 4)),
                    ),
                  ),
                ),
              ),
            ),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com#top") { s =>
          val hostEnd = afterHost(s, "example.com")
          val hAt = s.text.indexOf('#')
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoNone(span(s, hostEnd, hostEnd)),
            path = eoNone(span(s, hostEnd, hostEnd)),
            query = eoNone(span(s, hostEnd, hostEnd)),
            fragment = eoSome(
              Fragment(
                `#`("#", span(s, hAt, hAt + 1)),
                FragmentValue("top", spanOf(s, "top")),
              ),
            ),
          )
        },
        parsesTo(Url.parser, "https://api.example.com:443/v1/users?active=true#list") { s =>
          val pathStart = s.text.indexOf("/v1")
          val slash1 = s.text.indexOf('/', pathStart + 1)
          val qAt = s.text.indexOf('?')
          val hAt = s.text.indexOf('#')
          val eq = s.text.indexOf('=', qAt)
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "api.example.com"),
            port = eoSome(
              Port(
                `:`(":", span(s, s.text.indexOf(":443"), s.text.indexOf(":443") + 1)),
                PortNum("443", spanOf(s, "443"), 443),
              ),
            ),
            path = eoSome(
              PathWithSegs(
                PathSeg(`/`("/", span(s, pathStart, pathStart + 1)), PathSegment("v1", span(s, pathStart + 1, slash1))),
                eoSome(
                  PathRest(
                    `/`("/", span(s, slash1, slash1 + 1)),
                    eoSome(
                      PathRestSeg(
                        PathSegment("users", span(s, slash1 + 1, qAt)),
                        eoNone(span(s, qAt, qAt)),
                      ),
                    ),
                  ),
                ),
              ),
            ),
            query = eoSome(
              Query(
                QMark("?", span(s, qAt, qAt + 1)),
                QueryPair(
                  QueryKey("active", span(s, qAt + 1, eq)),
                  `=`("=", span(s, eq, eq + 1)),
                  QueryValue("true", span(s, eq + 1, hAt)),
                ),
                elementList[AndQueryPair](span(s, hAt, hAt))(),
              ),
            ),
            fragment = eoSome(
              Fragment(
                `#`("#", span(s, hAt, hAt + 1)),
                FragmentValue("list", spanOf(s, "list")),
              ),
            ),
          )
        },
      ),
      suite("invalid")(
        failsToParse(Url.parser, ""),
        failsToParse(Url.parser, "example.com"),
        failsToParse(Url.parser, "https://"),
        failsToParse(Url.parser, "://example.com"),
        failsToParse(Url.parser, "https:/example.com"),
        failsToParse(Url.parser, "https//example.com"),
        failsToParse(Url.parser, "https://example.com:"),
        failsToParse(Url.parser, "https://example.com?"),
        failsToParse(Url.parser, "https://example.com?x"),
        failsToParse(Url.parser, "https://example.com?="),
        failsToParse(Url.parser, "ht tp://example.com"),
        failsToParse(Url.parser, "http://127.0.0"),
        failsToParse(Url.parser, "http://127.0.0.1.2"),
        failsToParse(Url.parser, "http://256.0.0.1"),
      ),
    )

}
