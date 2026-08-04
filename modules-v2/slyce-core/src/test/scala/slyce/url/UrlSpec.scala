package slyce.url

import oxygen.predef.test.*

import slyce.TestUtils.*
import slyce.core.*
import slyce.core.builtIn.*
import slyce.url.model.*

object UrlSpec extends OxygenSpecDefault {

  /** Build a domain host from dotted labels, spanning the first occurrence of `hostText` in `s`. */
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

  private def bareUrl(s: Source, scheme: String, hostText: String): Url =
    Url(
      scheme = Scheme(scheme, spanOf(s, scheme)),
      sep = `://`("://", spanOf(s, "://")),
      host = domainHost(s, hostText),
      port = eoNone(eofSpan(s)),
      path = elementList[PathSeg](eofSpan(s))(),
      trailingSlash = eoNone(eofSpan(s)),
      query = eoNone(eofSpan(s)),
      fragment = eoNone(eofSpan(s)),
    )

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
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoSome(
              Port(
                `:`(":", spanOf(s, ":", from = s.text.indexOf("com") + 3)),
                PortNum("8080", spanOf(s, "8080"), 8080),
              ),
            ),
            path = elementList[PathSeg](eofSpan(s))(),
            trailingSlash = eoNone(eofSpan(s)),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com/a") { s =>
          val slash = s.text.indexOf('/')
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoNone(span(s, slash, slash)),
            path = elementList[PathSeg](eofSpan(s))(
              PathSeg(`/`("/", span(s, slash, slash + 1)), PathSegment("a", spanOf(s, "a"))),
            ),
            trailingSlash = eoNone(eofSpan(s)),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com/a/b") { s =>
          val slash0 = s.text.indexOf('/')
          val slash1 = s.text.indexOf('/', slash0 + 1)
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoNone(span(s, slash0, slash0)),
            path = elementList[PathSeg](eofSpan(s))(
              PathSeg(`/`("/", span(s, slash0, slash0 + 1)), PathSegment("a", span(s, slash0 + 1, slash1))),
              PathSeg(`/`("/", span(s, slash1, slash1 + 1)), PathSegment("b", span(s, slash1 + 1, s.length))),
            ),
            trailingSlash = eoNone(eofSpan(s)),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com/") { s =>
          val slash = s.text.lastIndexOf('/')
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoNone(span(s, slash, slash)),
            path = elementList[PathSeg](span(s, slash, slash))(),
            trailingSlash = eoSome(`/`("/", span(s, slash, slash + 1))),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com/a/") { s =>
          val slash0 = s.text.indexOf('/')
          val slash1 = s.text.lastIndexOf('/')
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoNone(span(s, slash0, slash0)),
            path = elementList[PathSeg](span(s, slash1, slash1))(
              PathSeg(`/`("/", span(s, slash0, slash0 + 1)), PathSegment("a", span(s, slash0 + 1, slash1))),
            ),
            trailingSlash = eoSome(`/`("/", span(s, slash1, slash1 + 1))),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "http://127.0.0.1") { s =>
          Url(
            scheme = Scheme("http", spanOf(s, "http")),
            sep = `://`("://", spanOf(s, "://")),
            host = ipv4Host(s, "127.0.0.1"),
            port = eoNone(eofSpan(s)),
            path = elementList[PathSeg](eofSpan(s))(),
            trailingSlash = eoNone(eofSpan(s)),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
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
            path = elementList[PathSeg](span(s, slash, slash))(),
            trailingSlash = eoSome(`/`("/", span(s, slash, slash + 1))),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com?x=1") { s =>
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoNone(spanOf(s, "?")),
            path = elementList[PathSeg](spanOf(s, "?"))(),
            trailingSlash = eoNone(spanOf(s, "?")),
            query = eoSome(
              Query(
                QMark("?", spanOf(s, "?")),
                QueryPair(
                  QueryKey("x", spanOf(s, "x")),
                  `=`("=", spanOf(s, "=")),
                  QueryValue("1", spanOf(s, "1")),
                ),
                elementList[AndQueryPair](eofSpan(s))(),
              ),
            ),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com?x=1&y=2") { s =>
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoNone(spanOf(s, "?")),
            path = elementList[PathSeg](spanOf(s, "?"))(),
            trailingSlash = eoNone(spanOf(s, "?")),
            query = eoSome(
              Query(
                QMark("?", spanOf(s, "?")),
                QueryPair(
                  QueryKey("x", spanOf(s, "x")),
                  `=`("=", spanOf(s, "=")),
                  QueryValue("1", spanOf(s, "1")),
                ),
                elementList[AndQueryPair](eofSpan(s))(
                  AndQueryPair(
                    `&`("&", spanOf(s, "&")),
                    QueryPair(
                      QueryKey("y", spanOf(s, "y")),
                      `=`("=", spanOf(s, "=", from = s.text.indexOf('&'))),
                      QueryValue("2", spanOf(s, "2")),
                    ),
                  ),
                ),
              ),
            ),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com#top") { s =>
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = domainHost(s, "example.com"),
            port = eoNone(spanOf(s, "#")),
            path = elementList[PathSeg](spanOf(s, "#"))(),
            trailingSlash = eoNone(spanOf(s, "#")),
            query = eoNone(spanOf(s, "#")),
            fragment = eoSome(
              Fragment(
                `#`("#", spanOf(s, "#")),
                FragmentValue("top", spanOf(s, "top")),
              ),
            ),
          )
        },
        parsesTo(Url.parser, "https://api.example.com:443/v1/users?active=true#list") { s =>
          val pathStart = s.text.indexOf("/v1")
          val qAt = s.text.indexOf('?')
          val hAt = s.text.indexOf('#')
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
            path = elementList[PathSeg](span(s, qAt, qAt))(
              PathSeg(`/`("/", span(s, pathStart, pathStart + 1)), PathSegment("v1", spanOf(s, "v1"))),
              PathSeg(`/`("/", spanOf(s, "/users")), PathSegment("users", spanOf(s, "users"))),
            ),
            trailingSlash = eoNone(span(s, qAt, qAt)),
            query = eoSome(
              Query(
                QMark("?", span(s, qAt, qAt + 1)),
                QueryPair(
                  QueryKey("active", spanOf(s, "active")),
                  `=`("=", spanOf(s, "=", from = qAt)),
                  QueryValue("true", spanOf(s, "true")),
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
