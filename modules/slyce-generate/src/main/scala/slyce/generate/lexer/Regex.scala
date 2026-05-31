package slyce.generate.lexer

import cats.data.NonEmptyList
import cats.syntax.option.*
import oxygen.core.InfiniteSet
import oxygen.predef.core.{unesc, IndentedString}
import scala.annotation.tailrec

import slyce.generate.*

sealed trait Regex {

  final def repeat(min: Int, max: Option[Int]): Regex =
    Regex.Repeat(this, min, max)

  final def optional: Regex =
    repeat(0, 1.some)

  final def exactlyN(n: Int): Regex =
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
          "Sequence:",
          IndentedString.indented(
            seq.map(_.toIdtStr),
          ),
        )
      case Regex.Group(seqs) =>
        IndentedString.inline(
          "Group:",
          IndentedString.indented(
            seqs.toList.map(_.toIdtStr),
          ),
        )
      case Regex.Repeat(reg, min, max) =>
        IndentedString.inline(
          s"Repeat($min, $max):",
          IndentedString.indented(
            reg.toIdtStr,
          ),
        )
    }

  private[Regex] def internalExhaustive: Option[NonEmptyList[String]]

  final def exhaustive: Option[NonEmptyList[String]] = internalExhaustive.map(_.distinct.sorted)

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

    override private[Regex] def internalExhaustive: Option[NonEmptyList[String]] = chars match {
      case InfiniteSet.Inclusive(explicit) if explicit.nonEmpty => NonEmptyList.fromListUnsafe(explicit.toList).map(_.toString).some
      case _                                                    => None
    }

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

  final case class Sequence(seq: List[Regex]) extends Regex {

    override private[Regex] def internalExhaustive: Option[NonEmptyList[String]] = {
      @tailrec
      def loop(seq: List[Regex], acc: NonEmptyList[String]): Option[NonEmptyList[String]] =
        seq match {
          case head :: tail =>
            head.internalExhaustive match {
              case Some(value) => loop(tail, crossStrings(acc, value))
              case None        => None
            }
          case Nil =>
            acc.some
        }

      loop(seq, NonEmptyList.one(""))
    }

  }
  object Sequence {

    def apply(regs: Regex*): Sequence =
      Sequence(regs.toList)

    def apply(str: String): Sequence =
      Sequence(str.map(CharClass.inclusive(_))*)

  }

  final case class Group(seqs: NonEmptyList[Sequence]) extends Regex {

    override private[Regex] def internalExhaustive: Option[NonEmptyList[String]] =
      seqs.traverse(_.internalExhaustive).map(_.flatMap(identity))

  }
  object Group {

    def apply(seq0: Sequence, seqN: Sequence*): Group =
      Group(NonEmptyList(seq0, seqN.toList))

  }

  final case class Repeat(reg: Regex, min: Int, max: Option[Int]) extends Regex {

    override private[Regex] def internalExhaustive: Option[NonEmptyList[String]] = {
      @tailrec
      def loop(
          min: Int,
          max: Int,
          baseExhaustive: NonEmptyList[String],
          acc: List[String],
          current: NonEmptyList[String],
      ): List[String] =
        if max < 0 then acc
        else {
          val newCurrent: NonEmptyList[String] = crossStrings(current, baseExhaustive)

          loop(
            min - 1,
            max - 1,
            baseExhaustive,
            if min <= 0 then acc ++ newCurrent.toList else acc,
            newCurrent,
          )
        }

      for {
        max <- max
        baseExhaustive <- reg.internalExhaustive
        res <- NonEmptyList.fromList(loop(min, max, baseExhaustive, Nil, NonEmptyList.one("")))
      } yield res
    }

  }

  private def crossStrings(s1: NonEmptyList[String], s2: NonEmptyList[String]): NonEmptyList[String] =
    for {
      s1 <- s1
      s2 <- s2
    } yield s1 + s2

}
