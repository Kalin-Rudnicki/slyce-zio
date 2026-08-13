package slyce.core

import oxygen.predef.core.*
import scala.Ordering.Implicits.infixOrderingOps

sealed trait Span {
  def optionalSource: Option[Source]
}
object Span {

  sealed trait HasSource extends Span {
    val source: Source
    override final def optionalSource: Option[Source] = source.some
  }

  sealed trait KnownPosition extends HasSource

  final case class Range(
      source: Source,
      startInclusive: Position,
      endExclusive: Position,
  ) extends Span.KnownPosition {

    def startIsEof: Boolean = startInclusive.zeroBasedAbsolute >= source.length
    def endIsEof: Boolean = endExclusive.zeroBasedAbsolute >= source.length
    def isNonEmpty: Boolean = endExclusive.zeroBasedAbsolute - startInclusive.zeroBasedAbsolute > 0

    def <>(that: Span.Range): Span.Range =
      Span.Range(
        source = this.source,
        startInclusive = this.startInclusive min that.startInclusive,
        endExclusive = this.endExclusive max that.endExclusive,
      )

  }

  final case class UnknownPosition(source: Source) extends Span.HasSource

  case object UnknownSource extends Span {
    override val optionalSource: Option[Source] = None
  }

}
