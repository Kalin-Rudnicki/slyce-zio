package slyce.generate.builder

import slyce.core.*
import slyce.generate.lexer.*

object Builders {

  final class LineNoCounter {
    private var _lineNo: Int = 1

    def newMode(): Unit = _lineNo += 2

    def calcLineNo(): Int = {
      val lineNo = _lineNo
      _lineNo += 1
      lineNo
    }

  }

  object lexer {

    def apply(startMode: String)(modes: (LineNoCounter ?=> LexerInput.Mode)*): LexerInput = {
      val lineNoCounter: LineNoCounter = new LineNoCounter
      LexerInput(
        Marked(startMode, Span.Unknown),
        modes.toList.map(_(using lineNoCounter)),
      )
    }

    object mode {

      def apply(modeName: String)(lines: (LineNoCounter ?=> LexerInput.Mode.Line)*)(using lineNoCounter: LineNoCounter): LexerInput.Mode = {
        lineNoCounter.newMode()
        LexerInput.Mode(
          Marked(modeName, Span.Unknown),
          lines.toList.map(_(using lineNoCounter)),
        )
      }

      def line(reg: Regex, toMode: Yields.ToMode[String] = Yields.ToMode.Same)(yields: Yields.Yield*)(using lineNoCounter: LineNoCounter): LexerInput.Mode.Line =
        LexerInput.Mode.Line(
          lineNoCounter.calcLineNo(),
          Marked(reg, Span.Unknown),
          Yields(
            yields.toList.map(Marked(_, Span.Unknown)),
            Marked(toMode, Span.Unknown),
          ),
        )

    }

  }

}
