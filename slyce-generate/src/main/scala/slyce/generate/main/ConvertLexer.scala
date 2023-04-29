package slyce.generate.main

import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*

import slyce.core.*
import slyce.generate.lexer.*
import slyce.generate.parsers.Lexer as CurrentLexer

object ConvertLexer {

  def convertLexer(lexer: CurrentLexer.NonTerminal.Lexer): LexerInput =
    LexerInput(
      startMode = lexer._2.markedText,
      modes = lexer._3.toNonEmptyList.toList.map(convertMode),
    )

  private def convertMode(mode: CurrentLexer.NonTerminal.Mode): LexerInput.Mode =
    LexerInput.Mode(
      name = mode._2.markedText,
      lines = mode._3.toNonEmptyList.toList.map(convertLine),
    )

  private def convertLine(line: CurrentLexer.NonTerminal.Line): LexerInput.Mode.Line =
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

  private def convertRegex(regex: CurrentLexer.NonTerminal.Regex): Regex =
    regex match {
      case CurrentLexer.NonTerminal.Regex._1(group)      => convertGroup(group)
      case CurrentLexer.NonTerminal.Regex._2(charClass)  => convertCharClass(charClass)
      case CurrentLexer.NonTerminal.Regex._3(reg, quant) => convertQuant(convertRegex(reg), quant)
    }

  private def convertGroup(group: CurrentLexer.NonTerminal.Group): Regex =
    Regex.Group(
      group.lift.toNonEmptyList.map(convertSequence),
    )

  private def convertSequence(sequence: CurrentLexer.NonTerminal.Sequence): Regex.Sequence =
    Regex.Sequence(
      sequence.toList.map(convertRegex),
    )

  private def convertCharClass(charClass: CurrentLexer.NonTerminal.CharClass): Regex.CharClass =
    charClass match {
      case CurrentLexer.NonTerminal.CharClass._1(_, isFlipped, ccChars, _) =>
        val base =
          Regex.CharClass.union(
            ccChars.toNonEmptyList.toList.map(convertCCChars) *,
          )

        if (isFlipped.toOption.nonEmpty) base.~
        else base
      case CurrentLexer.NonTerminal.CharClass._2(char) =>
        Regex.CharClass.inclusive(char.text.head)
      case CurrentLexer.NonTerminal.CharClass._3(escChar) =>
        Regex.CharClass.inclusive(convertEscChar(escChar))
      case CurrentLexer.NonTerminal.CharClass._4(escChars) =>
        convertEscChars(escChars)
    }

  private def convertEscChar(escChar: CurrentLexer.Terminal.escChar): Char =
    escChar.text(1) match {
      case 'n'  => '\n'
      case 't'  => '\t'
      case '\\' => '\\'
      case c    => c
    }

  private def convertEscChars(escChars: CurrentLexer.Terminal.escChars): Regex.CharClass =
    escChars.text match {
      case "\\d" => Regex.CharClass.`\\d`
      case "."   => Regex.CharClass.exclusive()
      case c     => throw RuntimeException(s"Invalid esc-chars: ${c.unesc} @ ${escChars.span}")
    }

  private def convertCCChars(ccChars: CurrentLexer.NonTerminal.CCChars): Regex.CharClass =
    ccChars match {
      case CurrentLexer.NonTerminal.CCChars._1(start, _, stop) =>
        Regex.CharClass.inclusiveRange(
          convertCCChar(start),
          convertCCChar(stop),
        )
      case CurrentLexer.NonTerminal.CCChars._2(ccChar)   => Regex.CharClass.inclusive(convertCCChar(ccChar))
      case CurrentLexer.NonTerminal.CCChars._3(escChars) => convertEscChars(escChars)
    }

  // TODO (KR) : Make this a `^`
  private def convertCCChar(ccChar: CurrentLexer.NonTerminal.CCChar): Char =
    ccChar match {
      case CurrentLexer.NonTerminal.CCChar._1(char)    => char.text.head
      case CurrentLexer.NonTerminal.CCChar._2(escChar) => convertEscChar(escChar)
    }

  private def convertQuant(regex: Regex, quant: CurrentLexer.NonTerminal.Quant): Regex =
    quant match {
      case _: CurrentLexer.NonTerminal.Quant._1                 => regex.optional
      case _: CurrentLexer.NonTerminal.Quant._2                 => regex.anyAmount
      case _: CurrentLexer.NonTerminal.Quant._3                 => regex.atLeastOnce
      case CurrentLexer.NonTerminal.Quant._4(_, exact, _)       => regex.exactlyN(exact.text.toInt)
      case CurrentLexer.NonTerminal.Quant._5(_, min, _, _)      => regex.atLeastN(min.text.toInt)
      case CurrentLexer.NonTerminal.Quant._6(_, _, max, _)      => regex.repeat(0, max.text.toInt.some)
      case CurrentLexer.NonTerminal.Quant._7(_, min, _, max, _) => regex.repeat(min.text.toInt, max.text.toInt.some)
    }

  private def convertYield(_yield: CurrentLexer.NonTerminal.Yield): Marked[Yields.Yield] = {
    val tmp: (((Option[Int], Option[Int])) => Yields.Yield, Span.Highlight) =
      _yield._1 match {
        case CurrentLexer.NonTerminal.YieldType._1(at) =>
          (
            Yields.Yield.Text(_),
            at.span,
          )
        case CurrentLexer.NonTerminal.YieldType._2(term) =>
          (
            Yields.Yield.Terminal(term.text, None, _),
            term.span,
          )
        case CurrentLexer.NonTerminal.YieldType._3(raw) =>
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

  private def convertRaw(raw: CurrentLexer.NonTerminal.Raw): String =
    raw.lift.toNonEmptyList.toList.map {
      case CurrentLexer.NonTerminal.Char._1(chars)   => chars.text
      case CurrentLexer.NonTerminal.Char._2(escChar) => convertEscChar(escChar).toString
    }.mkString

  private def convertSubString(
      subString: CurrentLexer.NonTerminal.SubString,
  ): ((Option[Int], Option[Int]), Option[Span.Highlight]) =
    subString match {
      case CurrentLexer.NonTerminal.SubString._1 =>
        ((None, None), None)
      case CurrentLexer.NonTerminal.SubString._2(_, exact, l) =>
        val exactInt = exact.text.toInt.some
        ((exactInt, exactInt), l.span.some)
      case CurrentLexer.NonTerminal.SubString._3(_, start, _, l) =>
        ((start.text.toInt.some, None), l.span.some)
      case CurrentLexer.NonTerminal.SubString._4(_, _, stop, l) =>
        ((None, stop.text.toInt.some), l.span.some)
      case CurrentLexer.NonTerminal.SubString._5(_, start, _, stop, l) =>
        ((start.text.toInt.some, stop.text.toInt.some), l.span.some)
    }

  private def convertToMode(
      source: Source,
      toMode: CurrentLexer.NonTerminal.ToMode,
  ): Marked[Yields.ToMode[String]] =
    toMode match {
      case CurrentLexer.NonTerminal.ToMode._1 =>
        Marked(
          Yields.ToMode.Same,
          Span.EOF(source),
        )
      case CurrentLexer.NonTerminal.ToMode._2(arrow, to) =>
        Marked(
          Yields.ToMode.Push(to.text),
          arrow.span <> to.span,
        )
      case CurrentLexer.NonTerminal.ToMode._3(arrow, to) =>
        Marked(
          Yields.ToMode.To(to.text),
          arrow.span <> to.span,
        )
      case CurrentLexer.NonTerminal.ToMode._4(arrow) =>
        Marked(
          Yields.ToMode.Pop,
          arrow.span,
        )
    }

}
