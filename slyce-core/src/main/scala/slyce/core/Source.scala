package slyce.core

import cats.data.NonEmptyList
import cats.syntax.list.*
import cats.syntax.option.*
import klib.utils.*
import scala.annotation.tailrec
import scala.math.Ordering.Implicits.infixOrderingOps
import zio.*

final case class Source(input: String, name: Option[String]) {
  val chars: List[Char] = input.toList
}
object Source {

  def fromFile(file: File): TaskM[Source] =
    file.readString.map(Source(_, file.toPath.toUri.toString.some))

  final case class Config(
      showName: Boolean,
      markerString: String,
      markerIndentString: String,
      eofMarkerString: String,
      eofMarkerIndentString: String,
      colors: NonEmptyList[ColorString.Color],
  )
  object Config {

    val Default: Config =
      Config(
        showName = true,
        // format: off
        markerString =          "    *** ",
        markerIndentString =    "     >  ",
        eofMarkerString =       "      * ",
        eofMarkerIndentString = "     >  ",
        // format: on
        colors = NonEmptyList
          .of(
            Color.Named.Red,
            Color.Named.Green,
            Color.Named.Yellow,
            Color.Named.Blue,
            Color.Named.Magenta,
            Color.Named.Cyan,
          )
          .map(c => ColorString.Color(c.some, None)),
      )

  }

}
