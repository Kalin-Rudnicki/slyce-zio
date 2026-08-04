package slyce.url.model

import slyce.core.*
import slyce.core.builtIn.*
import slyce.parse.*

// =====| Punctuation |=====

@regex("://".r) final case class `://`(text: String, span: Span.Range) extends Terminal
@regex(":".r) final case class `:`(text: String, span: Span.Range) extends Terminal
@regex("/".r) final case class `/`(text: String, span: Span.Range) extends Terminal
@regex("\\.".r) final case class `.`(text: String, span: Span.Range) extends Terminal
@regex("\\?".r) final case class QMark(text: String, span: Span.Range) extends Terminal
@regex("&".r) final case class `&`(text: String, span: Span.Range) extends Terminal
@regex("=".r) final case class `=`(text: String, span: Span.Range) extends Terminal
@regex("#".r) final case class `#`(text: String, span: Span.Range) extends Terminal

// =====| Url |=====

/** Practical URL subset: scheme '://' host [':' port] ['/' pathSeg]* ['/']? ['?' query] ['#' fragment]
  *
  * Examples: https://example.com https://example.com:8080/a/b?x=1&y=2#top http://127.0.0.1/
  */
final case class Url(
    scheme: Scheme,
    sep: `://`,
    host: Host,
    port: ElementOption[Port],
    path: ElementList[PathSeg],
    trailingSlash: ElementOption[`/`],
    query: ElementOption[Query],
    fragment: ElementOption[Fragment],
) extends NonTerminal {
  override val span: Span.Range = {
    val end =
      fragment.toOption
        .map(_.span)
        .orElse(query.toOption.map(_.span))
        .orElse(trailingSlash.toOption.map(_.span))
        .orElse(path.headOption.map { _ =>
          path match {
            case n: NonEmptyElementList[?] => n.span
            case n: ElementNil             => n.span
          }
        })
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

// =====| Host |=====

/** Host is either a dotted domain name (`a`.`b`.`c`) or an IPv4 address (`127`.`0`.`0`.`1`). */
sealed trait Host extends NonTerminal

/** Domain name: one or more labels separated by `.` (e.g. `localhost`, `example.com`, `api.example.com`). */
final case class DomainHost(
    head: DomainLabel,
    tail: ElementList[DotDomainLabel],
) extends Host {
  override val span: Span.Range =
    tail.headOption match {
      case Some(_) =>
        tail match {
          case n: NonEmptyElementList[?] => head.span <> n.span
          case n: ElementNil             => head.span
        }
      case None => head.span
    }
}

final case class DotDomainLabel(
    dot: `.`,
    label: DomainLabel,
) extends NonTerminal {
  override val span: Span.Range = dot.span <> label.span
}

@regex("[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-zA-Z0-9])?".r)
final case class DomainLabel(text: String, span: Span.Range) extends Terminal

/** IPv4: exactly four decimal octets separated by `.`. */
final case class Ipv4Host(
    a: Ipv4Octet,
    d1: `.`,
    b: Ipv4Octet,
    d2: `.`,
    c: Ipv4Octet,
    d3: `.`,
    d: Ipv4Octet,
) extends Host {
  override val span: Span.Range = a.span <> d.span
}

@regex("\\d{1,3}".r)
final case class Ipv4Octet(text: String, span: Span.Range, value: Int) extends Terminal
object Ipv4Octet {
  given BuildTerminal[Ipv4Octet] =
    (text, span) =>
      text.toIntOption match {
        case Some(v) if v >= 0 && v <= 255 => Right(Ipv4Octet(text, span, v))
        case Some(_)                      => Left(s"IPv4 octet out of range: $text")
        case None                         => Left(s"Invalid IPv4 octet: $text")
      }
}

// =====| Port / path / query / fragment |=====

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
