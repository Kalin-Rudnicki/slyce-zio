package slyce.core

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.list.*
import cats.syntax.option.*
import harness.core.*
import java.util.UUID
import monocle.Monocle.*
import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.matching.Regex

final case class Source(rawInput: String, name: Option[String]) { self =>
  val input: String = rawInput + "\n"
  private val uuid: UUID = UUID.randomUUID

  lazy val chars: List[Char] = input.toList
  val arr: IArray[Char] = IArray.unsafeFromArray(input.toArray)

  override def hashCode: Int = uuid.hashCode

  override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case source: Source => self.uuid == source.uuid
      case _              => false
    }

  def mark(
      messages: List[Marked[String]],
      config: Source.Config = Source.Config.Default,
  ): String =
    Source.mark(self, messages, config)

}
object Source {

  final case class Config(
      showName: Boolean,
      marker: Config.Marker,
      eofMarker: Config.Marker,
      colors: NonEmptyList[Color],
  )
  object Config {

    final case class Marker(
        start: String,
        cont: String,
    )

    val Default: Config =
      Config(
        showName = true,
        marker = Marker(
          "    *** ",
          "     >  ",
        ),
        eofMarker = Marker(
          "      * ",
          "     >  ",
        ),
        colors = NonEmptyList.of(
          Color.Named.Red,
          Color.Named.Green,
          Color.Named.Yellow,
          Color.Named.Blue,
          Color.Named.Magenta,
          Color.Named.Cyan,
        ),
      )

  }

  object mark {

    def apply(
        source: Source,
        msgs: List[Marked[String]],
        config: Config = Config.Default,
    ): String = {
      // NOTE : I don't think this being used incorrectly is worth forcing this function to return an error type.
      msgs.foreach { msg =>
        if (msg.span.optionalSource.exists(_ != source))
          throw new RuntimeException("`Source.mark` received marked messages not associated with source, consider using `Source.markAll`")
      }

      val cq1 = config.colors
      val (highlight, eof, unknown) = util.splitMessages(msgs)
      val (splitHighlights1, cq2) = util.splitHighlights(highlight, cq1)
      val splitHighlights2 = splitHighlights1.map(util.compileLines(source, _))

      val maxLineNoSize = highlight.map(_.span.end.lineNo).maxOption.getOrElse(0).toString.length

      val highlightStrings = splitHighlights2.map(util.markHighlights(source, maxLineNoSize, _, config))
      val eofStrings = pairEofColors(eof ::: unknown, config.colors, cq2).map { case (color, str) =>
        val a = s"${color.fgANSI}${config.eofMarker.start}${Color.Default.fgANSI}"
        val b = s"\n${color.fgANSI}${config.eofMarker.cont}${Color.Default.fgANSI}"
        str.split("\n").mkString(a, b, "")
      }

      List(
        source.name.map(s => s"[$s]:").toList,
        highlightStrings,
        Option.when(eofStrings.nonEmpty)("--- EOF ---").toList,
        eofStrings,
      ).flatten.mkString("\n")
    }

    private object util {

      // =====| Types |=====

      final case class Message(
          messages: NonEmptyList[String],
          span: Span.Highlight,
      ) {
        def spansMultipleLines: Boolean = span.start.lineNo != span.end.lineNo
      }
      object Message {
        def apply(message: String, span: Span.Highlight): Message = Message(NonEmptyList.one(message), span)
      }

      final case class ColoredMessage(
          messages: List[String],
          span: Span.Highlight,
          color: Color,
      ) { self =>
        def withMessage(other: Message): ColoredMessage = ColoredMessage(self.messages ::: other.messages.toList, span, color)
      }

      extension (self: List[ColoredMessage]) {
        def toLines: String =
          self.map { m => s"\n  - ${m.span.toString(true)}${m.messages.map(m2 => s"\n    * $m2").mkString}" }.mkString
      }

      // TODO (KR) : rename?
      final case class Line1(
          entering: Option[ColoredMessage],
          inLine: List[ColoredMessage],
          leaving: Option[ColoredMessage],
      ) {

        // TODO (KR) : REMOVE
        override def toString: String = {

          s"""entering:${entering.toList.toLines}
             |inLine:${inLine.toLines}
             |leaving:${leaving.toList.toLines}""".stripMargin
        }

      }

      final case class Line2(
          lineStart: Span.Pos,
          messages: NonEmptyList[ColoredMessage],
          lineEnd: Span.Pos,
      ) {

        override def toString: String =
          s"""sol: ${lineStart.toString(true)}
             |messages:${messages.toList.toLines}
             |eol: ${lineEnd.toString(true)}""".stripMargin

      }

      // =====| Functions |=====

      def splitMessages(messages: List[Marked[String]]): (
          List[Message],
          List[String],
          List[String],
      ) = {
        @tailrec
        def loop(
            queue: List[Marked[String]],
            rStack1: List[Message],
            rStack2: List[String],
            rStack3: List[String],
        ): (
            List[Message],
            List[String],
            List[String],
        ) =
          queue match {
            case head :: tail =>
              head.span match {
                case highlight: Span.Highlight => loop(tail, Message(head.value, highlight) :: rStack1, rStack2, rStack3)
                case _: Span.EOF               => loop(tail, rStack1, head.value :: rStack2, rStack3)
                case Span.Unknown              => loop(tail, rStack1, rStack2, head.value :: rStack3)
              }
            case Nil => (rStack1.reverse, rStack2.reverse, rStack3.reverse)
          }

        loop(messages, Nil, Nil, Nil)
      }

      def splitHighlights(messages: List[Message], allColors: NonEmptyList[Color]): (List[List[Line1]], NonEmptyList[Color]) = {
        @tailrec
        def loop(
            queue: List[Message],
            colorQueue: NonEmptyList[Color],
            entering: Option[ColoredMessage],
            rInLine: List[ColoredMessage],
            rCurrentStack: List[Line1],
            rLaterStack: List[Message],
            rFinishedStacks: List[List[Line1]],
        ): (List[List[Line1]], NonEmptyList[Color]) =
          queue match {
            case queueH :: queueT =>
              val cm = ColoredMessage(queueH.messages.toList, queueH.span, colorQueue.head)
              val nextColorQueue = colorQueue.tail.toNel.getOrElse(allColors)

              rInLine.headOption.orElse(entering) match {
                case Some(lastInLine) =>
                  if (queueH.span == lastInLine.span) {
                    val (newEntering, newRInLine) = (entering, rInLine) match {
                      case (_, rInLineH :: rInLineT) => (entering, rInLineH.withMessage(queueH) :: rInLineT)
                      case (Some(entering), _)       => (entering.withMessage(queueH).some, rInLine)
                      case _                         => throw new RuntimeException("Not possible...")
                    }
                    loop(queueT, colorQueue, newEntering, newRInLine, rCurrentStack, rLaterStack, rFinishedStacks)
                  } else if (queueH.span.start.absolutePos > lastInLine.span.end.absolutePos)
                    if (queueH.span.start.lineNo != lastInLine.span.end.lineNo)
                      loop(queue, colorQueue, None, Nil, Line1(entering, rInLine.reverse, None) :: rCurrentStack, rLaterStack, rFinishedStacks)
                    else if (queueH.spansMultipleLines)
                      loop(queueT, nextColorQueue, cm.some, Nil, Line1(entering, rInLine.reverse, cm.some) :: rCurrentStack, rLaterStack, rFinishedStacks)
                    else loop(queueT, nextColorQueue, entering, cm :: rInLine, rCurrentStack, rLaterStack, rFinishedStacks)
                  else
                    loop(queueT, colorQueue, entering, rInLine, rCurrentStack, queueH :: rLaterStack, rFinishedStacks)
                case None =>
                  if (queueH.spansMultipleLines) {
                    loop(queueT, nextColorQueue, cm.some, Nil, Line1(entering, rInLine.reverse, cm.some) :: rCurrentStack, rLaterStack, rFinishedStacks)
                  } else loop(queueT, nextColorQueue, entering, cm :: rInLine, rCurrentStack, rLaterStack, rFinishedStacks)
              }
            case Nil =>
              val newRCurrentStack =
                if (entering.nonEmpty || rInLine.nonEmpty) Line1(entering, rInLine.reverse, None) :: rCurrentStack
                else rCurrentStack
              val newRFinishedStacks =
                if (newRCurrentStack.nonEmpty) newRCurrentStack.reverse :: rFinishedStacks
                else rFinishedStacks
              if (rLaterStack.nonEmpty) loop(rLaterStack.reverse, colorQueue, None, Nil, Nil, Nil, newRFinishedStacks)
              else (newRFinishedStacks.reverse, colorQueue)
          }

        loop(messages.sortBy(_.span), allColors, None, Nil, Nil, Nil, Nil)
      }

      def compileLines(
          source: Source,
          lines: List[Line1],
      ): List[Line2] = {
        @tailrec
        def loop(
            queue: List[Line1],
            enteringPos: Option[(Span.Pos, Color, Span.Pos)],
            rStack: List[Line2],
        ): List[Line2] =
          enteringPos match {
            case Some((sol, color, eos)) if sol.lineNo != eos.lineNo =>
              val (eol, sonl) = Span.Pos.eolAndSonl(sol, source)
              val line2 = Line2(sol, NonEmptyList.one(ColoredMessage(Nil, Span.Highlight(sol, eol, source), color)), eol)
              loop(queue, (sonl, color, eos).some, line2 :: rStack)
            case _ =>
              queue match {
                case head :: tail =>
                  val firstPos = List(head.entering.map(_.span.end), head.inLine.map(_.span.start), head.leaving.map(_.span.start)).flatten.head
                  val sol = firstPos.atStartOfLine
                  val (eol, sonl) = Span.Pos.eolAndSonl(firstPos, source)

                  val spansInLine: List[ColoredMessage] =
                    List(
                      head.entering.map(_.focus(_.span.start).replace(sol)),
                      head.inLine,
                      head.leaving.map(_.focus(_.span.end).replace(eol).focus(_.messages).replace(Nil)), // messages will be displayed at the end
                    ).flatten

                  val line2 = Line2(sol, NonEmptyList.fromListUnsafe(spansInLine), eol)

                  loop(tail, head.leaving.map(cm => (sonl, cm.color, cm.span.end)), line2 :: rStack)
                case Nil =>
                  rStack.reverse
              }
          }

        loop(lines, None, Nil)
      }

      val colorRegex: Regex = "^([ \t]*)([^ \t\n]*(?:[ \t]+[^ \t\n]+)*)([ \t]*\n?)$".r
      def markHighlights(
          source: Source,
          maxLineNoSize: Int,
          lines: List[Line2],
          config: Config,
      ): String = {
        def rStackFromLine(line: Line2): List[String] = {
          @tailrec
          def loop2(
              start: Span.Pos,
              messages: List[ColoredMessage],
              rStack: List[String],
          ): List[String] =
            messages match {
              case head :: tail =>
                val plainSubStr = source.input.substring(start.inputIndex, head.span.start.inputIndex)
                val basicColorizedSubStr = source.input.substring(head.span.start.inputIndex, head.span.end.inputIndex + 1)
                val rColorizedSubStrs: List[String] =
                  basicColorizedSubStr match {
                    case colorRegex(a, b, c) =>
                      List(
                        if (c.nonEmpty) List(Color.Default.bgANSI, c.flatMap { case '\n' => "\\n"; case c => c.toString }, head.color.bgANSI)
                        else Nil,
                        List(Color.Default.fgANSI, b, head.color.fgANSI),
                        if (a.nonEmpty) List(Color.Default.bgANSI, a, head.color.bgANSI) else Nil,
                      ).flatten
                    case str => throw new RuntimeException(s"should not be possible...\n${str.unesc}")
                  }

                val newRStack = Color.Default.fgANSI :: rColorizedSubStrs ::: plainSubStr :: rStack

                if (head.span.end == line.lineEnd) newRStack
                else loop2(head.span.end.onChar(source.input(head.span.end.inputIndex)), tail, newRStack)
              case Nil =>
                source.input.substring(start.inputIndex, line.lineEnd.inputIndex + 1).stripSuffix("\n") :: rStack
            }

          val annotatedLine = loop2(line.lineStart, line.messages.toList, s"${line.lineStart.lineNo.toString.alignRight(maxLineNoSize)} : " :: Nil)

          annotatedLine.reverse ::: line.messages.toList.flatMap { cm =>
            val a = s"\n${cm.color.fgANSI}${config.marker.start}${Color.Default.fgANSI}"
            val b = s"\n${cm.color.fgANSI}${config.marker.cont}${Color.Default.fgANSI}"
            cm.messages.map { str => str.split("\n").mkString(a, b, "") }
          }
        }

        @tailrec
        def loop(
            queue: List[Line2],
            rStack: List[List[String]],
        ): String =
          queue match {
            case queueH :: queueT =>
              loop(queueT, rStackFromLine(queueH) :: rStack)
            case Nil =>
              rStack.reverse.map(_.mkString).mkString("\n")
          }

        loop(lines, Nil)
      }

    }

    def pairEofColors(messages: List[String], allColors: NonEmptyList[Color], colorQueue: NonEmptyList[Color]): List[(Color, String)] = {
      @tailrec
      def loop(
          messages: List[String],
          colorQueue: NonEmptyList[Color],
          rStack: List[(Color, String)],
      ): List[(Color, String)] =
        messages match {
          case head :: tail => loop(tail, colorQueue.tail.toNel.getOrElse(allColors), (colorQueue.head, head) :: rStack)
          case Nil          => rStack.reverse
        }

      loop(messages, colorQueue, Nil)
    }

  }

  // TODO (KR) : Improve
  def markAll(
      msgs: List[Marked[String]],
      config: Config = Config.Default,
  ): String = {
    val (unknownSource, knownSource) =
      msgs
        .groupBy(_.span.optionalSource)
        .toList
        .partitionMap { case (mSource, msgs) =>
          mSource match {
            case Some(source) => (source, msgs).asRight
            case None         => msgs.asLeft
          }
        }

    val fromKnown =
      knownSource.map { case (source, msgs) =>
        source.mark(msgs, config)
      }

    val fromUnknown = {
      val tmp = unknownSource.flatten
      Option.when(tmp.nonEmpty)(Source("", None).mark(tmp, config))
    }

    (fromKnown ++ fromUnknown).mkString("\n\n")
  }

}
