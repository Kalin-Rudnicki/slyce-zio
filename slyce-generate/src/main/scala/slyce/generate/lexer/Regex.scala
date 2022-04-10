package slyce.generate.lexer

import cats.data.NonEmptyList
import cats.syntax.option.*
import klib.utils.*

import slyce.generate.*

sealed trait Regex {

  final def repeat(min: Int, max: Option[Int]): Regex =
    Regex.Repeat(this, min, max)

  final def maybe: Regex =
    repeat(0, 1.some)

  final def exactly(n: Int): Regex =
    repeat(n, n.some)

  final def atLeastN(n: Int): Regex =
    repeat(n, None)

  final def anyAmount: Regex =
    atLeastN(0)

  final def atLeastOnce: Regex =
    atLeastN(1)

  final def toIdtStr: IndentedString =
    this match {
      case cc: Regex.CharClass =>
        cc.toString
      case Regex.Sequence(seq) =>
        IndentedString.inline(
          "Sequence",
          IndentedString.indented(
            seq.map(_.toIdtStr),
          ),
        )
      case Regex.Group(seqs) =>
        IndentedString.inline(
          "Group",
          IndentedString.indented(
            seqs.toList.map(_.toIdtStr),
          ),
        )
      case Regex.Repeat(reg, min, max) =>
        IndentedString.inline(
          s"Repeat($min, $max)",
          IndentedString.indented(
            reg.toIdtStr,
          ),
        )
    }

}

object Regex {

  final case class CharClass(chars: InfiniteSet[Char]) extends Regex {

    def ~ : CharClass =
      CharClass(this.chars.~)

    def |(that: CharClass): CharClass =
      CharClass(this.chars | that.chars)

    override def toString: String =
      chars match {
        case InfiniteSet.Inclusive(explicit) => explicit.prettyChars("Inclusive")
        case InfiniteSet.Exclusive(explicit) => explicit.prettyChars("Exclusive")
      }

  }

  object CharClass {

    // builders

    def union(charClasses: CharClass*): CharClass =
      CharClass(InfiniteSet.union(charClasses.map(_.chars)*))

    def inclusive(chars: Char*): CharClass =
      CharClass(InfiniteSet.Inclusive(chars.toSet))

    def inclusiveRange(start: Char, end: Char): CharClass =
      inclusive(start.to(end)*)

    def exclusive(chars: Char*): CharClass =
      CharClass(InfiniteSet.Exclusive(chars.toSet))

    def exclusiveRange(start: Char, end: Char): CharClass =
      exclusive(start.to(end)*)

    // constants

    val `[A-Z]` : CharClass = inclusiveRange('A', 'Z')
    val `[a-z]` : CharClass = inclusiveRange('a', 'z')
    val `\\d`: CharClass = inclusiveRange('0', '9')
    val `.` : CharClass = exclusive()

    val `[A-Za-z_\\d]` : CharClass = union(`[A-Z]`, `[a-z]`, inclusive('_'), `\\d`)

  }

  final case class Sequence(seq: List[Regex]) extends Regex
  object Sequence {

    def apply(regs: Regex*): Sequence =
      Sequence(regs.toList)

    def apply(str: String): Sequence =
      Sequence(str.map(CharClass.inclusive(_))*)

  }

  final case class Group(seqs: NonEmptyList[Sequence]) extends Regex
  object Group {

    def apply(seq0: Sequence, seqN: Sequence*): Group =
      Group(NonEmptyList(seq0, seqN.toList))

  }

  final case class Repeat(reg: Regex, min: Int, max: Option[Int]) extends Regex

}
