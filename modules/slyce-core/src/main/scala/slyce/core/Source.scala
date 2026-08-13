package slyce.core

import oxygen.core.Color
import oxygen.predef.core.*
import scala.collection.mutable

final case class Source(
    text: String,
    name: Option[String],
) {

  val chars: IArray[Char] = IArray.unsafeFromArray(text.toArray)
  val length: Int = text.length

  lazy val positions: IArray[Position] = {
    val positionsArray: Array[Position] = new Array[Position](length + 1)

    var pos: Position = Position.Start
    var idx: Int = 0

    positionsArray(idx) = pos

    while idx < length do {
      pos = pos.onChar(chars(idx))
      idx += 1
      positionsArray(idx) = pos
    }

    IArray.unsafeFromArray(positionsArray)
  }

  /** Pretty-print diagnostics for this source (carets under [[Span.Range]] messages). */
  def mark(
      messages: List[Marked[String]],
      config: Source.Config = Source.Config.Default,
  ): String =
    Source.mark(this, messages, config)

  override lazy val hashCode: Int = (text, name).hashCode

  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: Source => this eq that
    case _            => false

}
object Source {

  final case class Config(
      showName: Boolean,
      showAnsi: Boolean,
      marker: Config.Marker,
      eofMarker: Config.Marker,
      colors: List[Color.Concrete],
  )
  object Config {
    final case class Marker(start: String, cont: String)

    val Default: Config =
      Config(
        showName = true,
        showAnsi = true,
        marker = Marker("    *** ", "     >  "),
        eofMarker = Marker("      * ", "     >  "),
        colors = List(
          Color.Named.Red,
          Color.Named.Green,
          Color.Named.Yellow,
          Color.Named.Blue,
          Color.Named.Magenta,
          Color.Named.Cyan,
        ),
      )

    /** No ANSI escapes — for logs / tests. */
    val Plain: Config = Default.copy(showAnsi = false)
  }

  /** Render messages associated with a single [[source]]. Messages with a different source throw; use [[markAll]] for mixed sources. Messages without a source (unknown) are listed under an EOF-style
    * section.
    */
  def mark(
      source: Source,
      messages: List[Marked[String]],
      config: Config = Config.Default,
  ): String = {
    messages.foreach { msg =>
      msg.span.optionalSource.foreach { s =>
        if s != source then
          throw new RuntimeException(
            "`Source.mark` received marked messages not associated with source; use `Source.markAll`",
          )
      }
    }

    val (ranged, eofLike) =
      messages.partitionMap { m =>
        m.span match {
          case r: Span.Range if r.source eq source => Left((m.value, r))
          case _: Span.Range                       =>
            throw new RuntimeException(
              "`Source.mark` received a Range from another source; use `Source.markAll`",
            )
          case _ => Right(m.value)
        }
      }

    val body = markRanges(source, ranged, config)
    val eofBlock =
      if eofLike.isEmpty then ""
      else {
        val header = "--- EOF / unknown span ---"
        val lines = eofLike.zipWithIndex.map { case (msg, i) =>
          val color = config.colors(i % config.colors.length)
          formatEofMessage(msg, color, config)
        }
        (header :: lines).mkString("\n")
      }

    List(
      Option.when(config.showName)(source.name.map(n => s"[$n]:").getOrElse("[source]:")),
      Option.when(body.nonEmpty)(body),
      Option.when(eofBlock.nonEmpty)(eofBlock),
    ).flatten.mkString("\n")
  }

  /** Group messages by source and render each group; unknown-source messages last. */
  def markAll(
      messages: List[Marked[String]],
      config: Config = Config.Default,
  ): String = {
    val bySource = mutable.LinkedHashMap.empty[Option[Source], List[Marked[String]]]
    messages.foreach { m =>
      val key = m.span.optionalSource
      bySource.updateWith(key) {
        case Some(xs) => Some(xs :+ m)
        case None     => Some(List(m))
      }
    }

    bySource.toList
      .flatMap {
        case (Some(src), msgs) =>
          List(mark(src, msgs, config))
        case (None, msgs) =>
          val body = msgs.map(m => s"  - ${m.value}").mkString("\n")
          List(s"[unknown source]:\n$body")
      }
      .filter(_.nonEmpty)
      .mkString("\n\n")
  }

  // =====| internals |=====

  private final case class Hit(message: String, start: Int, end: Int, color: Color.Concrete)

  private def markRanges(
      source: Source,
      ranged: List[(String, Span.Range)],
      config: Config,
  ): String = {
    if ranged.isEmpty then return ""

    val hits =
      ranged.zipWithIndex.map { case ((msg, r), i) =>
        val start = r.startInclusive.zeroBasedAbsolute.min(source.length)
        val end = r.endExclusive.zeroBasedAbsolute.min(source.length).max(start)
        Hit(msg, start, end, config.colors(i % config.colors.length))
      }

    val lines = source.text.split("\n", -1).toList
    // line index -> absolute start of line
    val lineStarts: Array[Int] = {
      val arr = new Array[Int](lines.length)
      var abs = 0
      var i = 0
      while i < lines.length do {
        arr(i) = abs
        abs += lines(i).length + (if i < lines.length - 1 then 1 else 0)
        i += 1
      }
      arr
    }

    def lineOf(abs: Int): Int = {
      var lo = 0
      var hi = lineStarts.length - 1
      while lo < hi do {
        val mid = (lo + hi + 1) / 2
        if lineStarts(mid) <= abs then lo = mid else hi = mid - 1
      }
      lo
    }

    val maxLineNoW = (lineOf(hits.map(_.end).max) + 1).toString.length.max(1)
    val affectedLines =
      hits
        .flatMap(h => lineOf(h.start).to(lineOf(math.max(h.start, h.end - 1)).max(lineOf(h.start))))
        .distinct
        .sorted

    val out = mutable.ArrayBuffer.empty[String]
    affectedLines.foreach { li =>
      val lineAbs = lineStarts(li)
      val lineText = lines(li)
      val lineEndAbs = lineAbs + lineText.length
      val lineNo = (li + 1).toString
      val prefix = s"${" " * (maxLineNoW - lineNo.length)}$lineNo : "

      // paint line with per-hit colors (last hit wins on overlap)
      val painted = new StringBuilder
      painted.append(prefix)
      var i = 0
      while i < lineText.length do {
        val abs = lineAbs + i
        val covering = hits.filter(h => abs >= h.start && abs < h.end)
        covering.lastOption match {
          case Some(h) =>
            if config.showAnsi then painted.append(h.color.fgANSI)
            painted.append(lineText.charAt(i))
            if config.showAnsi then painted.append(Color.Default.fgANSI)
          case None =>
            painted.append(lineText.charAt(i))
        }
        i += 1
      }
      // EOF caret on empty end of last line
      if lineText.isEmpty then {
        val covering = hits.filter(h => h.start >= lineAbs && h.start <= lineEndAbs && h.start == h.end)
        covering.lastOption.foreach { h =>
          if config.showAnsi then painted.append(h.color.fgANSI)
          painted.append('⏎')
          if config.showAnsi then painted.append(Color.Default.fgANSI)
        }
      }
      out += painted.toString

      // underline row
      val caret = new StringBuilder
      caret.append(" " * prefix.length)
      var j = 0
      while j < lineText.length do {
        val abs = lineAbs + j
        if hits.exists(h => abs >= h.start && abs < h.end) then caret.append('^')
        else caret.append(' ')
        j += 1
      }
      if hits.exists(h => h.start == h.end && h.start >= lineAbs && h.start <= lineEndAbs) then caret.append('^')
      val caretStr = caret.toString
      if caretStr.exists(_ == '^') then out += caretStr

      // messages for hits that start on this line
      hits.filter(h => lineOf(h.start) == li).foreach { h =>
        out += formatMarkerMessage(h.message, h.color, config)
      }
    }
    out.mkString("\n")
  }

  private def formatMarkerMessage(msg: String, color: Color.Concrete, config: Config): String = {
    val start =
      if config.showAnsi then s"${color.fgANSI}${config.marker.start}${Color.Default.fgANSI}"
      else config.marker.start
    val cont =
      if config.showAnsi then s"${color.fgANSI}${config.marker.cont}${Color.Default.fgANSI}"
      else config.marker.cont
    msg.split("\n", -1).mkString(start, s"\n$cont", "")
  }

  private def formatEofMessage(msg: String, color: Color.Concrete, config: Config): String = {
    val start =
      if config.showAnsi then s"${color.fgANSI}${config.eofMarker.start}${Color.Default.fgANSI}"
      else config.eofMarker.start
    val cont =
      if config.showAnsi then s"${color.fgANSI}${config.eofMarker.cont}${Color.Default.fgANSI}"
      else config.eofMarker.cont
    msg.split("\n", -1).mkString(start, s"\n$cont", "")
  }

}
