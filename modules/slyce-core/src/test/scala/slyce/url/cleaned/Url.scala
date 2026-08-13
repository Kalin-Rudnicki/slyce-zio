package slyce.url.cleaned

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

/** Practical URL subset: scheme '://' host [':' port] [ absPath ] [ '?' query ] [ '#' fragment ]
  *
  * AbsPath is left-factored so `/` is not ambiguous between path segments and a trailing slash:
  *   - RootOnly: `/`
  *   - PathWithSegs: `/seg` (`/seg`)* `/`?
  */
final case class Url(
    scheme: Scheme,
    sep: `://`,
    host: Host,
    port: ElementOption[Port],
    path: ElementOption[AbsPath],
    query: ElementOption[Query],
    fragment: ElementOption[Fragment],
) extends NonTerminal {
  override val span: Span.Range = {
    val end =
      fragment.toOption
        .map(_.span)
        .orElse(query.toOption.map(_.span))
        .orElse(path.toOption.map(_.span))
        .orElse(port.toOption.map(_.span))
        .getOrElse(host.span)
    scheme.span <> end
  }
}
object Url {

  val parser: Parser[Url] = Parser.derived[Url](2)

}

@regex("[a-zA-Z][-a-zA-Z0-9+.]*".r)
final case class Scheme(text: String, span: Span.Range) extends Terminal

// =====| Host |=====

/** Host is either a dotted domain name or an IPv4 address. */
sealed trait Host extends NonTerminal

/** Domain name: one or more labels separated by `.`. Labels start with a letter so pure-numeric hosts go to [[Ipv4Host]] (LALR-friendly first-token split).
  */
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

@regex("[a-zA-Z][-a-zA-Z0-9]*".r)
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
        case Some(_)                       => Left(s"IPv4 octet out of range: $text")
        case None                          => Left(s"Invalid IPv4 octet: $text")
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

/** Absolute path after authority — fully left-factored on `/`: AbsPath ::= `/` | `/` seg PathRest? PathRest ::= `/` (seg PathRest?)? so a bare `/` is never ambiguous with `/seg`.
  */
sealed trait AbsPath extends NonTerminal

/** A single trailing `/` with no segments (e.g. `https://example.com/`). */
final case class RootOnly(slash: `/`) extends AbsPath {
  override val span: Span.Range = slash.span
}

/** At least one `/seg`, then optional further `/…` via [[PathRest]]. */
final case class PathWithSegs(
    head: PathSeg,
    more: ElementOption[PathRest],
) extends AbsPath {
  override val span: Span.Range =
    more.toOption.map(m => head.span <> m.span).getOrElse(head.span)
}

final case class PathSeg(
    slash: `/`,
    name: PathSegment,
) extends NonTerminal {
  override val span: Span.Range = slash.span <> name.span
}

/** Continuation after a segment: another `/` and optionally another segment (+ deeper rest). */
final case class PathRest(
    slash: `/`,
    after: ElementOption[PathRestSeg],
) extends NonTerminal {
  override val span: Span.Range =
    after.toOption.map(a => slash.span <> a.span).getOrElse(slash.span)
}

final case class PathRestSeg(
    name: PathSegment,
    more: ElementOption[PathRest],
) extends NonTerminal {
  override val span: Span.Range =
    more.toOption.map(m => name.span <> m.span).getOrElse(name.span)
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
