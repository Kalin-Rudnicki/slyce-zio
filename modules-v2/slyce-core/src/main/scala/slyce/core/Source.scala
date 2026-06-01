package slyce.core

final case class Source(
    text: String,
    name: Option[String],
) {

  val chars: IArray[Char] = IArray.unsafeFromArray(text.toArray)
  val length: Int = text.length

  lazy val positions: IArray[Position] = {
    val positionsArray: Array[Position] = new Array[Position](length + 1)

    var pos: Position = Position.Start
    var idx: Int = 0

    positionsArray(idx) = pos

    while idx < length do {
      pos = pos.onChar(chars(idx))
      idx += 1
      positionsArray(idx) = pos
    }

    IArray.unsafeFromArray(positionsArray)
  }

  override lazy val hashCode: Int = (text, name).hashCode

  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: Source => this eq that
    case _            => false

}
