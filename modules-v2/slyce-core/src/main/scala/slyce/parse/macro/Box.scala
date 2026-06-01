package slyce.parse.`macro`

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import scala.reflect.TypeTest

private[`macro`] sealed trait Box[T] {

  def set(value: T)(using Quotes): Unit

  def value(using Quotes): T

  // not sure if running in `sbt ~compile` mode will cause this to leak memory?
  def clearWith(f: T => Unit)(using Quotes): Unit

  final def narrow[T2 <: T](using tt: TypeTest[T, T2], t1Tag: TypeTag[T], t2Tag: TypeTag[T2]): Box[T2] = new Box.Narrow[T, T2](this, tt, t1Tag, t2Tag)

}
private[`macro`] object Box {

  def empty[T]: Box[T] = new Root[T]

  private final class Root[T] extends Box[T] {

    private var _value: Option[T] = None

    override def set(value: T)(using Quotes): Unit = {
      if _value.nonEmpty then report.errorAndAbort("internal defect, attempted to double-set box")
      _value = value.some
    }

    override def value(using Quotes): T =
      _value.getOrElse { report.errorAndAbort("internal defect, attempted to get value of box before it was set") }

    // not sure if running in `sbt ~compile` mode will cause this to leak memory?
    override def clearWith(f: T => Unit)(using Quotes): Unit = {
      // handle recursive
      val tmp = _value
      _value = None
      tmp.foreach(f)
    }

  }

  private final class Narrow[T1, T2 <: T1](parent: Box[T1], tt: TypeTest[T1, T2], t1Tag: TypeTag[T1], t2Tag: TypeTag[T2]) extends Box[T2] {

    override def set(value: T2)(using Quotes): Unit = parent.set(value)

    override def value(using Quotes): T2 = parent.value match
      case tt(value) => value
      case invalid   => report.errorAndAbort(s"Invalid Narrow ($t1Tag -> $t2Tag) : $invalid")

    override def clearWith(f: T2 => Unit)(using Quotes): Unit = parent.clearWith:
      case tt(value) => f(value)
      case invalid   => report.errorAndAbort(s"Invalid Narrow ($t1Tag -> $t2Tag) : $invalid")

  }

}
