package slyce.core

import oxygen.predef.core.*

/** Marks the position before the char:
  *
  * "ABC\nDEF"
  *
  * 0 @ 0:0 : "|ABC\nDEF"
  *
  * 1 @ 0:1 : "A|BC\nDEF"
  *
  * 2 @ 0:2 : "AB|C\nDEF"
  *
  * 3 @ 0:3 : "ABC|\nDEF"
  *
  * 4 @ 1:0 : "ABC\n|DEF"
  *
  * 5 @ 1:1 : "ABC\nD|EF"
  *
  * 6 @ 1:2 : "ABC\nDE|F"
  *
  * 7 @ 1:3 : "ABC\nDEF|"
  */
final case class Position private[core] (
    zeroBasedAbsolute: Int,
    zeroBasedLineNo: Int,
    zeroBasedPosInLine: Int,
) extends Showable {

  def onChar(char: Char): Position = char match
    case '\n' => Position(zeroBasedAbsolute + 1, zeroBasedLineNo + 1, 0)
    case _    => Position(zeroBasedAbsolute + 1, zeroBasedLineNo, zeroBasedPosInLine + 1)

  ///////  ///////////////////////////////////////////////////////////////

  def absolute: Int = zeroBasedAbsolute
  def lineNo: Int = zeroBasedLineNo
  def posInLine: Int = zeroBasedPosInLine
  def oneBasedAbsolute: Int = zeroBasedAbsolute + 1
  def oneBasedLineNo: Int = zeroBasedLineNo + 1
  def oneBasedPosInLine: Int = zeroBasedPosInLine + 1

  def showLocal: Text =
    str"${oneBasedLineNo.toText}:${oneBasedPosInLine.toText}"

  def show(showAbsolute: Boolean): Text =
    if showAbsolute then str"${oneBasedAbsolute.toText} @ $showLocal"
    else showLocal

  override def show: Text = showLocal

}
object Position {

  val Start: Position = Position(0, 0, 0)

  given Ordering[Position] = Ordering.by(_.zeroBasedAbsolute)

}
