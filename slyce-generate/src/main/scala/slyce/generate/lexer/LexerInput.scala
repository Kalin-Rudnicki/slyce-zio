package slyce.generate.lexer

import slyce.core.*
import slyce.generate.*

final case class LexerInput(
    startMode: Marked[String],
    modes: List[LexerInput.Mode],
)
object LexerInput {

  final case class Mode(
      name: Marked[String],
      lines: List[Mode.Line],
  )

  object Mode {

    final case class Line(
        lineNo: Int,
        regex: Marked[Regex],
        semicolonSpan: Span,
        yields: Yields[String],
    )

  }

}
