package slyce.url.model

import slyce.core.*
import slyce.core.builtIn.*
import slyce.parse.*

// =====| Punctuation |=====

@regex("://".r) final case class `://`(text: String, span: Span.Range) extends Terminal
@regex(":".r) final case class `:`(text: String, span: Span.Range) extends Terminal
@regex("/".r) final case class `/`(text: String, span: Span.Range) extends Terminal
@regex("\\?".r) final case class QMark(text: String, span: Span.Range) extends Terminal
@regex("&".r) final case class `&`(text: String, span: Span.Range) extends Terminal
@regex("=".r) final case class `=`(text: String, span: Span.Range) extends Terminal
@regex("#".r) final case class `#`(text: String, span: Span.Range) extends Terminal

// =====| Url |=====

/**
 * Practical URL subset:
 *   scheme '://' host [':' port] ['/' pathSeg]* ['?' query] ['#' fragment]
 *
 * Examples:
 *   https://example.com
 *   https://example.com:8080/a/b?x=1&y=2#top
 */
final case class Url(
    scheme: Scheme,
    sep: `://`,
    host: Host,
    port: ElementOption[Port],
    path: ElementList[PathSeg],
    query: ElementOption[Query],
    fragment: ElementOption[Fragment],
) extends NonTerminal {
  override val span: Span.Range = {
    val end =
      fragment.toOption.map(_.span)
        .orElse(query.toOption.map(_.span))
        .orElse(path.headOption.map { _ => path match { case n: NonEmptyElementList[?] => n.span; case n: ElementNil => n.span } })
        .orElse(port.toOption.map(_.span))
        .getOrElse(host.span)
    scheme.span <> end
  }
}
object Url {

  val parser: Parser[Url] =
    new Parser[Url] {
      override def parse(source: Source): Either[ParseError, Url] = ???
    }

}

@regex("[a-zA-Z][a-zA-Z0-9+.-]*".r)
final case class Scheme(text: String, span: Span.Range) extends Terminal

@regex("[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-zA-Z0-9])?)*".r)
final case class Host(text: String, span: Span.Range) extends Terminal

final case class Port(
    colon: `:`,
    number: PortNum,
) extends NonTerminal {
  override val span: Span.Range = colon.span <> number.span
}

@regex("\\d+".r)
final case class PortNum(text: String, span: Span.Range, value: Int) extends Terminal
object PortNum {
  given BuildTerminal[PortNum] = BuildTerminal.attemptDecode1(_.toInt)(PortNum.apply)
}

final case class PathSeg(
    slash: `/`,
    name: PathSegment,
) extends NonTerminal {
  override val span: Span.Range = slash.span <> name.span
}

@regex("[^/?#\\s]+".r)
final case class PathSegment(text: String, span: Span.Range) extends Terminal

final case class Query(
    q: QMark,
    head: QueryPair,
    tail: ElementList[AndQueryPair],
) extends NonTerminal {
  override val span: Span.Range =
    tail.headOption match {
      case Some(last) => q.span <> last.span
      case None       => q.span <> head.span
    }
}

final case class QueryPair(
    key: QueryKey,
    eq: `=`,
    value: QueryValue,
) extends NonTerminal {
  override val span: Span.Range = key.span <> value.span
}

final case class AndQueryPair(
    and: `&`,
    pair: QueryPair,
) extends NonTerminal {
  override val span: Span.Range = and.span <> pair.span
}

@regex("[^&=#\\s]+".r)
final case class QueryKey(text: String, span: Span.Range) extends Terminal

@regex("[^&=#\\s]*".r)
final case class QueryValue(text: String, span: Span.Range) extends Terminal

final case class Fragment(
    hash: `#`,
    value: FragmentValue,
) extends NonTerminal {
  override val span: Span.Range = hash.span <> value.span
}

@regex("[^\\s]*".r)
final case class FragmentValue(text: String, span: Span.Range) extends Terminal
