package slyce.parse.`macro`

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.collection.mutable
import scala.reflect.TypeTest

final class ExtractedTypeCache private () {

  private val cache: mutable.Map[TypeRepr, Box[ExtractedType]] = mutable.Map.empty

  def get(tpe: TypeRepr): (box: Box[ExtractedType], alreadyExists: Boolean) =
    cache.get(tpe) match {
      case Some(box) => (box, true)
      case None      =>
        val box = Box.empty[ExtractedType]
        cache.update(tpe, box)
        (box, false)
    }

  def getNarrowed[T <: ExtractedType: TypeTag](tpe: TypeRepr)(using tt: TypeTest[ExtractedType, T]): (Box[T], Boolean) = {
    val (box, alreadyExists) = get(tpe)
    (box.narrow[T], alreadyExists)
  }

  // TODO (KR) : foreach(box.clearWith)
  //           : just use weak ref? <-- really not sure about this.. can it clear out from under you then, if everything is weak?

}
object ExtractedTypeCache {

  def empty: ExtractedTypeCache = new ExtractedTypeCache

}
