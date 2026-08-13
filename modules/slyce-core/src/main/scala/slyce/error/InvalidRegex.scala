package slyce.error

import oxygen.predef.core.*

import slyce.parse.LexerInput

final case class InvalidRegex(input: LexerInput, hint: String) {
  override def toString: String =
    s"Invalid regex \\${input.source.text}\\ (${input.read.fold("eof")(t => s"char ${t._1.unesc}")} at ${input.sourceLoc}): $hint"
}
