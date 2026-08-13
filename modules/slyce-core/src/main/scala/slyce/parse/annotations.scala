package slyce.parse

import scala.annotation.Annotation
import scala.util.matching.Regex

import slyce.core.*

final case class regex(r: Regex) extends Annotation

final case class ignoreBefore[A <: Terminal]() extends Annotation
final case class ignoreAfter[A <: Terminal]() extends Annotation
final case class ignoreBetween[A <: Terminal]() extends Annotation
