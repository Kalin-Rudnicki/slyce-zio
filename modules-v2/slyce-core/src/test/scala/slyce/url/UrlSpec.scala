package slyce.url

import oxygen.predef.test.*

import slyce.TestUtils.*
import slyce.core.*
import slyce.core.builtIn.*
import slyce.url.model.*

object UrlSpec extends OxygenSpecDefault {

  private def bareUrl(s: Source, scheme: String, host: String): Url =
    Url(
      scheme = Scheme(scheme, spanOf(s, scheme)),
      sep = `://`("://", spanOf(s, "://")),
      host = Host(host, spanOf(s, host)),
      port = eoNone(eofSpan(s)),
      path = elementList[PathSeg](eofSpan(s))(),
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
            host = Host("example.com", spanOf(s, "example.com")),
            port = eoSome(
              Port(
                `:`(":", spanOf(s, ":", from = s.text.indexOf("com") + 3)),
                PortNum("8080", spanOf(s, "8080"), 8080),
              ),
            ),
            path = elementList[PathSeg](eofSpan(s))(),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com/a") { s =>
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = Host("example.com", spanOf(s, "example.com")),
            port = eoNone(span(s, s.text.indexOf('/'), s.text.indexOf('/'))),
            path = elementList[PathSeg](eofSpan(s))(
              PathSeg(`/`("/", spanOf(s, "/")), PathSegment("a", spanOf(s, "a"))),
            ),
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
            host = Host("example.com", spanOf(s, "example.com")),
            port = eoNone(span(s, slash0, slash0)),
            path = elementList[PathSeg](eofSpan(s))(
              PathSeg(`/`("/", span(s, slash0, slash0 + 1)), PathSegment("a", span(s, slash0 + 1, slash1))),
              PathSeg(`/`("/", span(s, slash1, slash1 + 1)), PathSegment("b", span(s, slash1 + 1, s.length))),
            ),
            query = eoNone(eofSpan(s)),
            fragment = eoNone(eofSpan(s)),
          )
        },
        parsesTo(Url.parser, "https://example.com?x=1") { s =>
          Url(
            scheme = Scheme("https", spanOf(s, "https")),
            sep = `://`("://", spanOf(s, "://")),
            host = Host("example.com", spanOf(s, "example.com")),
            port = eoNone(spanOf(s, "?")),
            path = elementList[PathSeg](spanOf(s, "?"))(),
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
            host = Host("example.com", spanOf(s, "example.com")),
            port = eoNone(spanOf(s, "?")),
            path = elementList[PathSeg](spanOf(s, "?"))(),
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
            host = Host("example.com", spanOf(s, "example.com")),
            port = eoNone(spanOf(s, "#")),
            path = elementList[PathSeg](spanOf(s, "#"))(),
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
            host = Host("api.example.com", spanOf(s, "api.example.com")),
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
      ),
    )

}
