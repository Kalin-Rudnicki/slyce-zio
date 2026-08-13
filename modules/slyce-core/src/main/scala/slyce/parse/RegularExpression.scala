package slyce.parse

import oxygen.core.InfiniteSet
import oxygen.predef.core.*
import scala.annotation.tailrec

import slyce.core.*
import slyce.error.InvalidRegex

sealed trait RegularExpression {

  // =====| Combinators |=====

  final def repeat(min: Int, max: Option[Int]): RegularExpression =
    RegularExpression.Repeat(this, min, max)

  final def optional: RegularExpression =
    repeat(0, 1.some)

  final def exactlyN(n: Int): RegularExpression =
    repeat(n, n.some)

  final def atLeastN(n: Int): RegularExpression =
    repeat(n, None)

  final def anyAmount: RegularExpression =
    atLeastN(0)

  final def atLeastOnce: RegularExpression =
    atLeastN(1)

  final def >>(that: RegularExpression): RegularExpression.Sequence =
    RegularExpression.Sequence(this, that)

  // =====|  |=====

  final def sequenceElems: List[RegularExpression.NonSequence] = this match
    case RegularExpression.Sequence(seq)      => seq
    case regex: RegularExpression.NonSequence => regex :: Nil

  final def removeRedundantGroups: RegularExpression = this match
    case RegularExpression.Group(NonEmptyList(head, Nil)) => head.removeRedundantGroups
    case RegularExpression.Group(options)                 => RegularExpression.Group(options.map(seq => RegularExpression.Sequence(seq.removeRedundantGroups)))
    case RegularExpression.Sequence(elems)                => RegularExpression.Sequence(elems.map(_.removeRedundantGroups)*)
    case RegularExpression.Repeat(reg, min, max)          => RegularExpression.Repeat(reg.removeRedundantGroups, min, max)
    case cc: RegularExpression.CharClass                  => cc

  // =====|  |=====

  final def toIdtStr: IndentedString = this match
    case cc: RegularExpression.CharClass =>
      cc.toString
    case RegularExpression.Sequence(seq) =>
      IndentedString.section("Sequence:")(
        seq.map(_.toIdtStr),
      )
    case RegularExpression.Group(options) =>
      IndentedString.section("Group:")(
        options.toList.map(_.toIdtStr),
      )
    case RegularExpression.Repeat(reg, min, max) =>
      IndentedString.section(s"Repeat($min, ${max.getOrElse("infinity")}):")(
        reg.toIdtStr,
      )

}
object RegularExpression {

  sealed trait NonSequence extends RegularExpression

  final case class CharClass(chars: InfiniteSet[Char]) extends RegularExpression.NonSequence {

    def ~ : CharClass =
      CharClass(this.chars.~)

    def |(that: CharClass): CharClass =
      CharClass(this.chars | that.chars)

    override def toString: String = chars.toString

  }
  object CharClass {

    // builders

    def union(charClasses: CharClass*): CharClass =
      CharClass(InfiniteSet.unionAll(charClasses.map(_.chars)*))

    def inclusive(chars: Char*): CharClass =
      CharClass(InfiniteSet.Inclusive(chars.toSet))

    def inclusiveRange(start: Char, end: Char): CharClass =
      inclusive(start.to(end)*)

    def exclusive(chars: Char*): CharClass =
      CharClass(InfiniteSet.Exclusive(chars.toSet))

    def exclusiveRange(start: Char, end: Char): CharClass =
      exclusive(start.to(end)*)

    // constants

    val `[A-Z]`: CharClass = inclusiveRange('A', 'Z')
    val `[a-z]`: CharClass = inclusiveRange('a', 'z')
    val `\\d`: CharClass = inclusiveRange('0', '9')
    val `.`: CharClass = exclusive()

    val `[A-Za-z_\\d]`: CharClass = union(`[A-Z]`, `[a-z]`, inclusive('_'), `\\d`)

  }

  final case class Sequence(elems: List[RegularExpression.NonSequence]) extends RegularExpression
  object Sequence {

    def apply(regs: RegularExpression*): Sequence =
      Sequence(regs.toList.flatMap(_.sequenceElems))

    def apply(str: String): Sequence =
      Sequence(str.map(CharClass.inclusive(_))*)

  }

  // TODO (KR) : support capturing/non-capturing groups
  final case class Group(options: NonEmptyList[RegularExpression.Sequence]) extends RegularExpression.NonSequence
  object Group {

    def apply(option0: RegularExpression, optionN: RegularExpression*): Group =
      Group(NonEmptyList(option0, optionN.toList).map(RegularExpression.Sequence(_)))

  }

  // TODO (KR) : support lazy/greedy
  final case class Repeat(reg: RegularExpression, min: Int, max: Option[Int]) extends RegularExpression.NonSequence

  // =====| Parsing |=====

  // --- States ---

  private sealed trait ParseCharClassState {

    val negated: Boolean

    private def rawChars(input: LexerInput): Either[InvalidRegex, Set[Char]] = this match
      case ParseCharClassState.Empty(_, chars)          => chars.asRight
      case ParseCharClassState.SeenChar(_, chars, char) => (chars + char).asRight
      case ParseCharClassState.SeenCharRange(_, _, _)   => fail(input, "expected closure of char-class range")

    final def charClass(input: LexerInput): Either[InvalidRegex, CharClass] =
      this.rawChars(input).map { chars =>
        if negated then CharClass(InfiniteSet.Exclusive(chars))
        else CharClass(InfiniteSet.Inclusive(chars))
      }

    final def onChar(c: Char): ParseCharClassState = this match
      case ParseCharClassState.Empty(negated, chars) =>
        ParseCharClassState.SeenChar(negated, chars, c)
      case ParseCharClassState.SeenChar(negated, chars, char) =>
        ParseCharClassState.SeenChar(negated, chars + char, c)
      case ParseCharClassState.SeenCharRange(negated, chars, rangeStartChar) =>
        ParseCharClassState.Empty(negated, chars ++ rangeStartChar.to(c))

  }
  private object ParseCharClassState {
    final case class Empty(negated: Boolean, chars: Set[Char]) extends ParseCharClassState
    final case class SeenChar(negated: Boolean, chars: Set[Char], char: Char) extends ParseCharClassState
    final case class SeenCharRange(negated: Boolean, chars: Set[Char], rangeStartChar: Char) extends ParseCharClassState
  }

  private sealed trait ParseQuantState {

    final def onInt(i: Int): ParseQuantState = this match
      case ParseQuantState.Initial              => ParseQuantState.ParsingMin(i)
      case ParseQuantState.ParsingMin(min)      => ParseQuantState.ParsingMin(min * 10 + i)
      case ParseQuantState.SeenComma(min)       => ParseQuantState.ParsingMax(min, i)
      case ParseQuantState.ParsingMax(min, max) => ParseQuantState.ParsingMax(min, max * 10 + i)

  }
  private object ParseQuantState {
    case object Initial extends ParseQuantState
    final case class ParsingMin(min: Int) extends ParseQuantState
    final case class SeenComma(min: Int) extends ParseQuantState
    final case class ParsingMax(min: Int, max: Int) extends ParseQuantState
  }

  // --- Helpers ---

  private object ++: {
    def unapply(input: LexerInput): Option[(Char, LexerInput)] = input.read
  }

  private object eof {
    def unapply(input: LexerInput): Option[Unit] = Option.when(input.read.isEmpty)(())
  }

  private object unescChar {

    def apply(char: Char): Char = char match
      case 't' => '\t'
      case 'n' => '\n'
      case c   => c

    def unapply(char: Char): Option[Char] = unescChar(char).some

  }

  private object int {
    def unapply(char: Char): Option[Int] =
      Option.when(char >= '0' && char <= '9')(char.toInt - '0'.toInt)
  }

  private object charClassGroup {
    // TODO (KR) : include others here
    def unapply(char: Char): Option[Set[Char]] = char match
      case 'd' => '0'.to('9').toSet.some
      case _   => None
  }

  private val emptyGroup: RegularExpression.Group = RegularExpression.Group(RegularExpression.Sequence())

  extension (rStack: NonEmptyList[RegularExpression.Group]) {

    private def mapHeadGroup(f: RegularExpression.Group => RegularExpression.Group): NonEmptyList[RegularExpression.Group] =
      NonEmptyList(f(rStack.head), rStack.tail)

    private def traverseHeadGroup(f: RegularExpression.Group => Either[InvalidRegex, RegularExpression.Group]): Either[InvalidRegex, NonEmptyList[RegularExpression.Group]] =
      f(rStack.head).map(NonEmptyList(_, rStack.tail))

    private def modifyLastElem(input: LexerInput)(f: RegularExpression => RegularExpression): Either[InvalidRegex, NonEmptyList[RegularExpression.Group]] =
      traverseHeadGroup {
        _.popLast match {
          case Some((partialGroup, lastElem)) => (partialGroup <+ f(lastElem)).asRight
          case None                           => fail(input, "no last elem to modify")
        }
      }

  }
  extension (group: RegularExpression.Group) {

    private def <+(regex: RegularExpression): RegularExpression.Group = {
      val reversedOptions = group.options.reverse
      RegularExpression.Group(NonEmptyList(reversedOptions.head >> regex, reversedOptions.tail).reverse)
    }

    private def appendOption: RegularExpression.Group =
      RegularExpression.Group(group.options :+ RegularExpression.Sequence())

    private def popLast: Option[(RegularExpression.Group, RegularExpression)] = {
      val reversedOptions = group.options.reverse
      reversedOptions.head.elems.reverse match {
        case head :: tail => (RegularExpression.Group(NonEmptyList(Sequence(tail.reverse), reversedOptions.tail).reverse), head).some
        case Nil          => None
      }
    }

  }

  private def fail(input: LexerInput, message: String): Either[InvalidRegex, Nothing] =
    InvalidRegex(input, message).asLeft

  private def invalidInput(input: LexerInput): Either[InvalidRegex, Nothing] =
    fail(input, "unexpected input")

  // --- Parse Functions ---

  @tailrec
  private def parseCharClass(input: LexerInput, state: ParseCharClassState): Either[InvalidRegex, (CharClass, LexerInput)] =
    (input, state) match {
      case ('-' ++: nextInput, ParseCharClassState.SeenChar(negated, chars, char))                        => parseCharClass(nextInput, ParseCharClassState.SeenCharRange(negated, chars, char))
      case (']' ++: nextInput, _)                                                                         => state.charClass(input).map((_, nextInput))
      case ('\\' ++: charClassGroup(c) ++: nextInput, ParseCharClassState.Empty(negated, chars))          => parseCharClass(nextInput, ParseCharClassState.Empty(negated, chars ++ c))
      case ('\\' ++: charClassGroup(c) ++: nextInput, ParseCharClassState.SeenChar(negated, chars, char)) => parseCharClass(nextInput, ParseCharClassState.Empty(negated, chars + char ++ c))
      case ('\\' ++: unescChar(c) ++: nextInput, _)                                                       => parseCharClass(nextInput, state.onChar(c))
      case (c ++: nextInput, _)                                                                           => parseCharClass(nextInput, state.onChar(c))
      case _                                                                                              => invalidInput(input)
    }

  @tailrec
  private def parseQuant(input: LexerInput, state: ParseQuantState): Either[InvalidRegex, ((Int, Option[Int]), LexerInput)] =
    (input, state) match {
      case (int(i) ++: nextInput, _)                                 => parseQuant(nextInput, state.onInt(i))
      case (',' ++: nextInput, ParseQuantState.ParsingMin(min))      => parseQuant(nextInput, ParseQuantState.SeenComma(min))
      case ('}' ++: nextInput, ParseQuantState.ParsingMin(min))      => ((min, min.some), nextInput).asRight
      case ('}' ++: nextInput, ParseQuantState.SeenComma(min))       => ((min, None), nextInput).asRight
      case ('}' ++: nextInput, ParseQuantState.ParsingMax(min, max)) => ((min, max.some), nextInput).asRight
      case _                                                         => invalidInput(input)
    }

  @tailrec
  private def parseRegex(input: LexerInput, rStack: NonEmptyList[RegularExpression.Group]): Either[InvalidRegex, RegularExpression] =
    input match {
      // group
      case '(' ++: '?' ++: ':' ++: nextInput =>
        parseRegex(nextInput, emptyGroup :: rStack)
      case '(' ++: nextInput =>
        parseRegex(nextInput, emptyGroup :: rStack)
      case ')' ++: nextInput =>
        rStack.toList match {
          case g1 :: g2 :: gN => parseRegex(nextInput, NonEmptyList(g2 <+ g1, gN))
          case _              => fail(input, "no group to close")
        }
      case '|' ++: nextInput => parseRegex(nextInput, rStack.mapHeadGroup(_.appendOption))

      // repeat
      case '?' ++: nextInput =>
        rStack.modifyLastElem(input)(_.optional) match {
          case Right(newRStack) => parseRegex(nextInput, newRStack)
          case Left(error)      => error.asLeft
        }
      case '+' ++: nextInput =>
        rStack.modifyLastElem(input)(_.atLeastOnce) match {
          case Right(newRStack) => parseRegex(nextInput, newRStack)
          case Left(error)      => error.asLeft
        }
      case '*' ++: nextInput =>
        rStack.modifyLastElem(input)(_.anyAmount) match {
          case Right(newRStack) => parseRegex(nextInput, newRStack)
          case Left(error)      => error.asLeft
        }
      case '{' ++: nextInput =>
        parseQuant(nextInput, ParseQuantState.Initial) match {
          case Right(((min, max), nextInput2)) =>
            rStack.modifyLastElem(nextInput2)(_.repeat(min, max)) match {
              case Right(newRStack) => parseRegex(nextInput2, newRStack)
              case Left(error)      => error.asLeft
            }
          case Left(error) => error.asLeft
        }

      // char-class
      case '.' ++: nextInput =>
        parseRegex(nextInput, rStack.mapHeadGroup(_ <+ RegularExpression.CharClass.exclusive()))
      case '[' ++: nextInput =>
        val (negated, ccInput) = nextInput match
          case '^' ++: nextInput => (true, nextInput)
          case _                 => (false, nextInput)
        parseCharClass(ccInput, ParseCharClassState.Empty(negated, Set.empty)) match
          case Right((cc, nextInput)) => parseRegex(nextInput, rStack.mapHeadGroup(_ <+ cc))
          case Left(error)            => error.asLeft
      case '\\' ++: charClassGroup(c) ++: nextInput =>
        parseRegex(nextInput, rStack.mapHeadGroup(_ <+ CharClass.inclusive(c.toSeq*)))
      case '\\' ++: unescChar(c) ++: nextInput =>
        parseRegex(nextInput, rStack.mapHeadGroup(_ <+ CharClass.inclusive(c)))
      case c ++: nextInput =>
        parseRegex(nextInput, rStack.mapHeadGroup(_ <+ CharClass.inclusive(c)))

      // other
      case eof(_) =>
        rStack match {
          case NonEmptyList(head, Nil) => head.removeRedundantGroups.asRight
          case _                       => fail(input, "unclosed groups")
        }
      case _ => invalidInput(input)
    }

  def parse(source: Source): Either[InvalidRegex, RegularExpression] =
    parseRegex(LexerInput.fromSource(source), NonEmptyList.one(emptyGroup))

}
