package slyce.generate.main

import cats.syntax.option.*

import slyce.core.*
import slyce.generate.lexer.*
import slyce.generate.parsers.Lexer

object ConvertLexer {

  def convertLexer(lexer: Lexer.NonTerminal.Lexer): LexerInput =
    LexerInput(
      startMode = lexer._2.markedText,
      modes = lexer._3.toNonEmptyList.toList.map(convertMode),
    )

  private def convertMode(mode: Lexer.NonTerminal.Mode): LexerInput.Mode =
    LexerInput.Mode(
      name = mode._2.markedText,
      lines = mode._3.toNonEmptyList.toList.map(convertLine),
    )

  private def convertLine(line: Lexer.NonTerminal.Line): LexerInput.Mode.Line =
    LexerInput.Mode.Line(
      lineNo = line._2.span.start.lineNo,
      regex = Marked(
        Regex.Group(
          line._1.toNonEmptyList.map(convertSequence),
        ),
        line._2.span, // TODO (KR) : Use the correct span
      ),
      yields = Yields(
        yields = line._3.toList.map(convertYield),
        toMode = convertToMode(line._2.span.source, line._4),
      ),
    )

  private def convertRegex(regex: Lexer.NonTerminal.Regex): Regex =
    regex match {
      case Lexer.NonTerminal.Regex._1(group)      => convertGroup(group)
      case Lexer.NonTerminal.Regex._2(charClass)  => convertCharClass(charClass)
      case Lexer.NonTerminal.Regex._3(reg, quant) => convertQuant(convertRegex(reg), quant)
    }

  private def convertGroup(group: Lexer.NonTerminal.Group): Regex =
    Regex.Group(
      group.lift.toNonEmptyList.map(convertSequence),
    )

  private def convertSequence(sequence: Lexer.NonTerminal.Sequence): Regex.Sequence =
    Regex.Sequence(
      sequence.toList.map(convertRegex),
    )

  private def convertCharClass(charClass: Lexer.NonTerminal.CharClass): Regex.CharClass =
    charClass match {
      case Lexer.NonTerminal.CharClass._1(_, isFlipped, ccChars, _) =>
        val base =
          Regex.CharClass.union(
            ccChars.toNonEmptyList.toList.map(convertCCChars) *,
          )

        if (isFlipped.toOption.nonEmpty) base.~
        else base
      case Lexer.NonTerminal.CharClass._2(char) =>
        Regex.CharClass.inclusive(char.text.head)
      case Lexer.NonTerminal.CharClass._3(escChar) =>
        Regex.CharClass.inclusive(convertEscChar(escChar))
      case Lexer.NonTerminal.CharClass._4(escChars) =>
        convertEscChars(escChars)
    }

  private def convertEscChar(escChar: Lexer.Terminal.escChar): Char =
    escChar.text.head match {
      case 'n'  => '\n'
      case 't'  => '\t'
      case '\\' => '\\'
      case c    => c
    }

  private def convertEscChars(escChars: Lexer.Terminal.escChars): Regex.CharClass =
    escChars.text.head match {
      case 'd' => Regex.CharClass.`\\d`
      case _   => ???
    }

  private def convertCCChars(ccChars: Lexer.NonTerminal.CCChars): Regex.CharClass =
    ccChars match {
      case Lexer.NonTerminal.CCChars._1(start, _, stop) =>
        Regex.CharClass.inclusiveRange(
          convertCCChar(start),
          convertCCChar(stop),
        )
      case Lexer.NonTerminal.CCChars._2(ccChar)   => Regex.CharClass.inclusive(convertCCChar(ccChar))
      case Lexer.NonTerminal.CCChars._3(escChars) => convertEscChars(escChars)
    }

  // TODO (KR) : Make this a `^`
  private def convertCCChar(ccChar: Lexer.NonTerminal.CCChar): Char =
    ccChar match {
      case Lexer.NonTerminal.CCChar._1(char)    => char.text.head
      case Lexer.NonTerminal.CCChar._2(escChar) => convertEscChar(escChar)
    }

  private def convertQuant(regex: Regex, quant: Lexer.NonTerminal.Quant): Regex =
    quant match {
      case _: Lexer.NonTerminal.Quant._1                 => regex.optional
      case _: Lexer.NonTerminal.Quant._2                 => regex.anyAmount
      case _: Lexer.NonTerminal.Quant._3                 => regex.atLeastOnce
      case Lexer.NonTerminal.Quant._4(_, exact, _)       => regex.exactlyN(exact.text.toInt)
      case Lexer.NonTerminal.Quant._5(_, min, _, _)      => regex.atLeastN(min.text.toInt)
      case Lexer.NonTerminal.Quant._6(_, _, max, _)      => regex.repeat(0, max.text.toInt.some)
      case Lexer.NonTerminal.Quant._7(_, min, _, max, _) => regex.repeat(min.text.toInt, max.text.toInt.some)
    }

  private def convertYield(_yield: Lexer.NonTerminal.Yield): Marked[Yields.Yield] = {
    val tmp: (((Option[Int], Option[Int])) => Yields.Yield, Span.Highlight) =
      _yield._1 match {
        case Lexer.NonTerminal.YieldType._1(at) =>
          (
            Yields.Yield.Text(_),
            at.span,
          )
        case Lexer.NonTerminal.YieldType._2(term) =>
          (
            Yields.Yield.Terminal(term.text, None, _),
            term.span,
          )
        case Lexer.NonTerminal.YieldType._3(raw) =>
          (
            Yields.Yield.ConstText(convertRaw(raw), _),
            raw._1.span,
          )
      }

    val (build, h1) = tmp
    val (subStr, h2) = convertSubString(_yield._2)

    Marked(
      build(subStr),
      h1 <> h2,
    )
  }

  private def convertRaw(raw: Lexer.NonTerminal.Raw): String =
    raw.lift.toNonEmptyList.toList.map {
      case Lexer.NonTerminal.Char._1(chars)   => chars.text
      case Lexer.NonTerminal.Char._2(escChar) => convertEscChar(escChar).toString
    }.mkString

  private def convertSubString(
      subString: Lexer.NonTerminal.SubString,
  ): ((Option[Int], Option[Int]), Option[Span.Highlight]) =
    subString match {
      case Lexer.NonTerminal.SubString._1 =>
        ((None, None), None)
      case Lexer.NonTerminal.SubString._2(_, exact, l) =>
        val exactInt = exact.text.toInt.some
        ((exactInt, exactInt), l.span.some)
      case Lexer.NonTerminal.SubString._3(_, start, _, l) =>
        ((start.text.toInt.some, None), l.span.some)
      case Lexer.NonTerminal.SubString._4(_, _, stop, l) =>
        ((None, stop.text.toInt.some), l.span.some)
      case Lexer.NonTerminal.SubString._5(_, start, _, stop, l) =>
        ((start.text.toInt.some, stop.text.toInt.some), l.span.some)
    }

  private def convertToMode(
      source: Source,
      toMode: Lexer.NonTerminal.ToMode,
  ): Marked[Yields.ToMode[String]] =
    toMode match {
      case Lexer.NonTerminal.ToMode._1 =>
        Marked(
          Yields.ToMode.Same,
          Span.EOF(source),
        )
      case Lexer.NonTerminal.ToMode._2(arrow, to) =>
        Marked(
          Yields.ToMode.Push(to.text),
          arrow.span <> to.span,
        )
      case Lexer.NonTerminal.ToMode._3(arrow, to) =>
        Marked(
          Yields.ToMode.To(to.text),
          arrow.span <> to.span,
        )
      case Lexer.NonTerminal.ToMode._4(arrow) =>
        Marked(
          Yields.ToMode.Pop,
          arrow.span,
        )
    }

}
