package slyce.parse

import oxygen.predef.core.*

import slyce.core.*

final case class LexerInput private (source: Source, sourceLoc: Position) {

  val sourceIdx: Int = sourceLoc.zeroBasedAbsolute

  def read: Option[(Char, LexerInput)] =
    if sourceIdx >= source.length then None
    else {
      val c = source.chars(sourceIdx)
      Some((c, LexerInput(source, sourceLoc.onChar(c))))
    }

}
object LexerInput {
  def fromSource(source: Source): LexerInput = LexerInput(source, Position.Start)
}
