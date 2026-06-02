package slyce.parse.`macro`

import oxygen.meta.k0.ProductGeneric
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

import slyce.core.Span
import slyce.parse.*

private[slyce] object DeriveBuildTerminal {

  sealed trait ArgType
  object ArgType {

    sealed trait Known extends ArgType

    sealed trait Static extends ArgType.Known
    case object SpanArg extends ArgType.Static
    case object StringArg extends ArgType.Static

    final class Unknown(val gen: ProductGeneric.CaseClassGeneric[?])(val field: gen.Field[?]) extends ArgType

    def from(gen: ProductGeneric.CaseClassGeneric[?])(field: gen.Field[?])(using Quotes): ArgType =
      field.typeRepr.asType match
        case '[String]     => ArgType.StringArg
        case '[Span.Range] => ArgType.SpanArg
        case _             => Unknown(gen)(field)

  }

  def derivedImpl[A: Type](parentPos: Position, isAuto: Boolean, gen: ProductGeneric.CaseClassGeneric[A])(using Quotes): Expr[BuildTerminal[A]] = {
    val stringTypeRepr: TypeRepr = TypeRepr.of[String]
    val spanTypeRepr: TypeRepr = TypeRepr.of[Span.Range]

    val args: List[ArgType] = gen.fields.toList.map(ArgType.from(gen))

    val (unknown, known): (List[ArgType.Unknown], List[ArgType.Known]) =
      args.partitionMap { case arg: ArgType.Known => arg.asRight; case arg: ArgType.Unknown => arg.asLeft }

    def throwInvalid: Nothing =
      report.errorAndAbort(
        s"Unable to auto-derive BuildTerminal[${gen.typeRepr.showAnsiCode}], expected constructor of exactly (String, Span.Range) or (Span.Range, String). Provide your own given instance.",
        gen.pos,
      )

    if unknown.nonEmpty then throwInvalid

    known match {
      case ArgType.StringArg :: ArgType.SpanArg :: Nil =>
        '{
          new BuildTerminal[A] {
            override def build(text: String, span: Span.Range): Either[String, A] =
              ${ gen.instantiate.fieldsToInstance(List('text, 'span)) }.asRight
          }
        }
      case ArgType.SpanArg :: ArgType.StringArg :: Nil =>
        '{
          new BuildTerminal[A] {
            override def build(text: String, span: Span.Range): Either[String, A] =
              ${ gen.instantiate.fieldsToInstance(List('span, 'text)) }.asRight
          }
        }
      case _ => throwInvalid
    }
  }

  def derivedImpl[A: Type](using Quotes): Expr[BuildTerminal[A]] = {
    val gen: ProductGeneric.CaseClassGeneric[A] = ProductGeneric.CaseClassGeneric.of[A]
    derivedImpl[A](Position.ofMacroExpansion, false, gen)
  }

}
