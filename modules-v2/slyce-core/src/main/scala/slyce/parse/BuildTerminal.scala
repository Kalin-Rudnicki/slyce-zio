package slyce.parse

import oxygen.predef.core.*
import scala.util.{Failure, Success, Try}

import slyce.core.*

trait BuildTerminal[A] {
  def build(text: String, span: Span.Range): Either[String, A]
}
object BuildTerminal {

  def attemptDecode1[A, B](f: String => A)(b: (String, Span.Range, A) => B): BuildTerminal[B] = { (text, span) =>
    Try { f(text) } match
      case Success(value)     => b(text, span, value).asRight
      case Failure(exception) => exception.safeGetMessage.asLeft
  }

}
