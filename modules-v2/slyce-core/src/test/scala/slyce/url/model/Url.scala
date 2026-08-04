package slyce.url.model

import slyce.core.*
import slyce.core.builtIn.*
import slyce.parse.*
import slyce.url.cleaned as cleaned

// =====| Punctuation |=====

@regex("://".r) final case class `://`(text: String, span: Span.Range) extends Terminal
@regex(":".r) final case class `:`(text: String, span: Span.Range) extends Terminal
@regex("/".r) final case class `/`(text: String, span: Span.Range) extends Terminal
@regex("\\.".r) final case class `.`(text: String, span: Span.Range) extends Terminal
@regex("\\?".r) final case class QMark(text: String, span: Span.Range) extends Terminal
@regex("&".r) final case class `&`(text: String, span: Span.Range) extends Terminal
@regex("=".r) final case class `=`(text: String, span: Span.Range) extends Terminal
@regex("#".r) final case class `#`(text: String, span: Span.Range) extends Terminal

// =====| Url (desired / human-sensible AST) |=====

/**
 * Desired surface AST for URLs (human-sensible):
 *   scheme '://' host [':' port] ['/' pathSeg]* ['/']? ['?' query] ['#' fragment]
 *
 * Intentionally **not** LALR-safe as written (path list vs trailing `/`, host alternatives, …).
 * `Parser.derived[Url]` must fail at compile time until auto-rewrite exists.
 * Use [[slyce.url.cleaned.Url]] + [[fromCleaned]] for a working parser in the meantime.
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

  /**
   * RED by design: desired AST is not a valid surface grammar for derivation.
   * Expect compile error from [[GrammarValidity]] (path/trailingSlash FIRST overlap, host terminal overlap, …).
   */
  val parser: Parser[Url] = Parser.derived[Url](2)

  def fromCleaned(u: cleaned.Url): Url = {
    val (path, trailingSlash) = pathAndTrailFromCleaned(u.path)
    Url(
      scheme = Scheme(u.scheme.text, u.scheme.span),
      sep = `://`(u.sep.text, u.sep.span),
      host = hostFromCleaned(u.host),
      port = mapOpt(u.port)(portFromCleaned),
      path = path,
      trailingSlash = trailingSlash,
      query = mapOpt(u.query)(queryFromCleaned),
      fragment = mapOpt(u.fragment)(fragmentFromCleaned),
    )
  }

  private def emptySpan(near: Span.Range): Span.Range =
    Span.Range(near.source, near.endExclusive, near.endExclusive)

  private def mapOpt[A <: Element, B <: Element](opt: ElementOption[A])(f: A => B): ElementOption[B] =
    opt match {
      case ElementOption.Some(v)  => ElementOption.Some(f(v))
      case ElementOption.None(sp) => ElementOption.None(sp)
    }

  private def hostFromCleaned(h: cleaned.Host): Host =
    h match {
      case d: cleaned.DomainHost =>
        DomainHost(
          DomainLabel(d.head.text, d.head.span),
          mapList(d.tail) { dd =>
            DotDomainLabel(
              `.`(dd.dot.text, dd.dot.span),
              DomainLabel(dd.label.text, dd.label.span),
            )
          },
        )
      case i: cleaned.Ipv4Host =>
        Ipv4Host(
          Ipv4Octet(i.a.text, i.a.span, i.a.value),
          `.`(i.d1.text, i.d1.span),
          Ipv4Octet(i.b.text, i.b.span, i.b.value),
          `.`(i.d2.text, i.d2.span),
          Ipv4Octet(i.c.text, i.c.span, i.c.value),
          `.`(i.d3.text, i.d3.span),
          Ipv4Octet(i.d.text, i.d.span, i.d.value),
        )
    }

  private def portFromCleaned(p: cleaned.Port): Port =
    Port(
      `:`(p.colon.text, p.colon.span),
      PortNum(p.number.text, p.number.span, p.number.value),
    )

  /** Flatten left-factored AbsPath into path segments + optional trailing slash. */
  private def pathAndTrailFromCleaned(
      path: ElementOption[cleaned.AbsPath],
  ): (ElementList[PathSeg], ElementOption[`/`]) =
    path match {
      case ElementOption.None(sp) =>
        (ElementNil(sp), ElementOption.None(sp))
      case ElementOption.Some(cleaned.RootOnly(slash)) =>
        (
          ElementNil(Span.Range(slash.span.source, slash.span.startInclusive, slash.span.startInclusive)),
          ElementOption.Some(`/`(slash.text, slash.span)),
        )
      case ElementOption.Some(cleaned.PathWithSegs(head, more)) =>
        val headSeg = pathSegFromCleaned(head)
        more match {
          case ElementOption.None(sp) =>
            (NonEmptyElementList(headSeg, ElementNil(sp)), ElementOption.None(sp))
          case ElementOption.Some(rest) =>
            val (tailSegs, trail) = fromPathRest(rest)
            (consAll(headSeg, tailSegs), trail)
        }
    }

  private def fromPathRest(rest: cleaned.PathRest): (List[PathSeg], ElementOption[`/`]) =
    rest.after.toOption match {
      case None =>
        (Nil, ElementOption.Some(`/`(rest.slash.text, rest.slash.span)))
      case Some(cleaned.PathRestSeg(name, more)) =>
        val seg = PathSeg(`/`(rest.slash.text, rest.slash.span), PathSegment(name.text, name.span))
        more.toOption match {
          case None =>
            (seg :: Nil, ElementOption.None(emptySpan(seg.span)))
          case Some(next) =>
            val (tail, trail) = fromPathRest(next)
            (seg :: tail, trail)
        }
    }

  private def pathSegFromCleaned(s: cleaned.PathSeg): PathSeg =
    PathSeg(`/`(s.slash.text, s.slash.span), PathSegment(s.name.text, s.name.span))

  private def consAll(head: PathSeg, tail: List[PathSeg]): ElementList[PathSeg] = {
    def go(xs: List[PathSeg]): ElementList[PathSeg] =
      xs match {
        case Nil       => ElementNil(Span.Range(head.span.source, head.span.startInclusive, head.span.startInclusive))
        case h :: Nil  => NonEmptyElementList(h, ElementNil(emptySpan(h.span)))
        case h :: rest => NonEmptyElementList(h, go(rest))
      }
    go(head :: tail)
  }

  private def mapList[A <: Element, B <: Element](list: ElementList[A])(f: A => B): ElementList[B] =
    list match {
      case ElementNil(sp)            => ElementNil(sp)
      case NonEmptyElementList(h, t) => NonEmptyElementList(f(h), mapList(t)(f))
    }

  private def queryFromCleaned(q: cleaned.Query): Query =
    Query(
      QMark(q.q.text, q.q.span),
      queryPairFromCleaned(q.head),
      mapList(q.tail) { a =>
        AndQueryPair(`&`(a.and.text, a.and.span), queryPairFromCleaned(a.pair))
      },
    )

  private def queryPairFromCleaned(p: cleaned.QueryPair): QueryPair =
    QueryPair(
      QueryKey(p.key.text, p.key.span),
      `=`(p.eq.text, p.eq.span),
      QueryValue(p.value.text, p.value.span),
    )

  private def fragmentFromCleaned(f: cleaned.Fragment): Fragment =
    Fragment(
      `#`(f.hash.text, f.hash.span),
      FragmentValue(f.value.text, f.value.span),
    )

}

@regex("[a-zA-Z][-a-zA-Z0-9+.]*".r)
final case class Scheme(text: String, span: Span.Range) extends Terminal

// =====| Host |=====

/** Host is either a dotted domain name (`a`.`b`.`c`) or an IPv4 address (`127`.`0`.`0`.`1`). */
sealed trait Host extends NonTerminal

/** Domain name: one or more labels separated by `.`. */
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

@regex("[a-zA-Z0-9][-a-zA-Z0-9]*".r)
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
