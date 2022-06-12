package slyce.core

import cats.syntax.list.*
import cats.syntax.option.*
import scala.annotation.tailrec

sealed trait Span {

  val optionalSource: Option[Source]

  def toString(showAbsolute: Boolean): String =
    this match {
      case Span.Highlight(start, end, _) => s"Highlight(${start.toString(showAbsolute)} -> ${end.toString(showAbsolute)})"
      case Span.EOF(_)                   => "EOF"
      case Span.Unknown                  => "Unknown"
    }

  override def toString: String =
    toString(false)

}

object Span {
  sealed trait HasSource extends Span {
    val source: Source
    override val optionalSource: Option[Source] = source.some
  }

  final case class Highlight(
      start: Span.Pos,
      end: Span.Pos,
      source: Source,
  ) extends HasSource {

    def <>(other: Highlight): Highlight = Highlight(this.start, other.end, this.source)
    def <>(other: Option[Highlight]): Highlight =
      other match {
        case Some(other) => this <> other
        case None        => this
      }

  }

  final case class EOF(
      source: Source,
  ) extends HasSource

  case object Unknown extends Span {
    override val optionalSource: Option[Source] = None
  }

  def apply(start: Pos, end: Pos, source: Source): Highlight =
    Highlight(start, end, source)

  def apply(source: Source): EOF =
    EOF(source)

  def apply(): Unknown.type =
    Unknown

  /*
      NOTE : Do not be a buffoon and call these with spans from different sources...
   */

  def joinHighlightsNE(span0: Highlight, spanN: Highlight*): Highlight = {
    val all = span0 :: spanN.toList
    Highlight(
      all.minBy(_.start).start,
      all.maxBy(_.end).end,
      span0.source,
    )
  }

  def joinHasSourcesNE(span0: HasSource, spanN: HasSource*): HasSource = {
    val all = span0 :: spanN.toList
    val highlights = all.flatMap { case highlight: Highlight => highlight.some; case _ => None }
    highlights.toNel match {
      case Some(highlights) => joinHighlightsNE(highlights.head, highlights.tail*)
      case None             => span0
    }
  }

  def joinSpans(spans: Span*): Span = {
    val all = spans.toList
    val hasSources = all.flatMap { case hasSource: HasSource => hasSource.some; case _ => None }
    hasSources.toNel match {
      case Some(hasSources) => joinHasSourcesNE(hasSources.head, hasSources.tail*)
      case None             => Unknown
    }
  }

  final case class Pos private[core] (
      absolutePos: Int,
      lineNo: Int,
      posInLine: Int,
  ) {

    def onChar(c: Char): Pos =
      c match {
        case '\n' => Pos(absolutePos + 1, lineNo + 1, Pos.PosInLineStart)
        case _    => Pos(absolutePos + 1, lineNo, posInLine + 1)
      }

    def onStr(str: String): Pos = {
      @tailrec
      def loop(pos: Pos, chars: List[Char]): Pos =
        chars match {
          case head :: tail => loop(pos.onChar(head), tail)
          case Nil          => pos
        }

      loop(this, str.toList)
    }

    def toString(showAbsolute: Boolean): String =
      s"${if (showAbsolute) s"$absolutePos @ " else ""}$lineNo:$posInLine"

    override def toString: String =
      toString(false)

  }

  object Pos {

    private val AbsolutePosStart = 1
    private val LineNoStart = 1
    private val PosInLineStart = 1

    val Start: Pos = Pos(AbsolutePosStart, LineNoStart, PosInLineStart)

    implicit val posOrdering: Ordering[Pos] = Ordering.by(_.absolutePos)

  }

}
