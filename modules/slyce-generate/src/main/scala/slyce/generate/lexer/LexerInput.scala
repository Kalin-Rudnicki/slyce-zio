package slyce.generate.lexer

import oxygen.predef.core.*

import slyce.core.*
import slyce.generate.*

final case class LexerInput(
    startMode: Marked[String],
    modes: List[LexerInput.Mode],
) {

  lazy val atYields: List[String] = modes.flatMap(_.atYields).distinct.sorted

}
object LexerInput {

  final case class Mode(
      name: Marked[String],
      lines: List[Mode.Line],
  ) {

    def atYields: List[String] = lines.flatMap(_.atYields)

  }

  object Mode {

    final case class Line(
        lineNo: Int,
        regex: Marked[Regex],
        semicolonSpan: Span,
        yields: Yields[String],
    ) {

      private def calcIdx(str: String, posNegIdx: Int): Option[Int] = {
        val tmp1: Int = if posNegIdx >= 0 then posNegIdx else str.length + 1 + posNegIdx
        Option.when(tmp1 >= 0 && tmp1 <= str.length)(tmp1)
      }

      private def calcSubStr(str: String, start: Option[Int], end: Option[Int]): Option[String] =
        if start.isEmpty && end.isEmpty then str.some
        else
          for {
            startIdx <- calcIdx(str, start.getOrElse(0))
            endIdx <- calcIdx(str, end.getOrElse(-1)) if endIdx > startIdx
          } yield str.substring(startIdx, endIdx)

      // TODO (KR) : this should probably return a fallible value. If this doesnt succeed, then the `@` is not yieldable anyway...
      def atYields: List[String] = {
        val distinctAtYields: List[(Option[Int], Option[Int])] =
          yields.yields.flatMap {
            _.value match {
              case Yields.Yield.Text(subString) => subString.some
              case _                            => None
            }
          }.distinct

        if distinctAtYields.nonEmpty then
          regex.value.exhaustive match {
            case None             => Nil
            case Some(exhaustive) =>
              for {
                (min, max) <- distinctAtYields
                str <- exhaustive.toList
                res <- calcSubStr(str, min, max)
              } yield res
          }
        else Nil
      }

    }

  }

}
