package slyce.generate

import oxygen.quoted.Position

import slyce.parse.RegularExpression

final case class ParsedRegex(
    regexText: String,
    regex: RegularExpression,
    pos: Position,
) {
  val path: String = pos.sourceFile.path
  val lineNo: Int = pos.startLine
}
