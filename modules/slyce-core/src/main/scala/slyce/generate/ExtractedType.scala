package slyce.generate

import java.util.UUID
import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

import slyce.core.{Position as _, *}
import slyce.core.builtIn.*
import slyce.parse.*

private[slyce] sealed trait ExtractedType {

  final val typeId: ExtractedType.TypeId = ExtractedType.TypeId.random()

  ///////  ///////////////////////////////////////////////////////////////

  private var _lifecycle: ExtractedType.Lifecycle = ExtractedType.Lifecycle.Uninitialized

  protected def initializeInternal(parentPos: Position, cache: ExtractedTypeCache)(using Quotes): Unit

  ///////  ///////////////////////////////////////////////////////////////

  final def initialize(parentPos: Position, cache: ExtractedTypeCache)(using Quotes): Unit =
    _lifecycle match {
      case ExtractedType.Lifecycle.Uninitialized =>
        _lifecycle = ExtractedType.Lifecycle.Initialized
        initializeInternal(parentPos, cache)
      case ExtractedType.Lifecycle.Initialized =>
        ()
    }

  def typeRepr: TypeRepr

  def renderInline: String

  override def toString: String = renderInline

}
private[slyce] object ExtractedType {

  opaque type TypeId = UUID
  object TypeId {
    private[ExtractedType] final def random(): TypeId = UUID.randomUUID()
  }

  private enum Lifecycle {
    case Uninitialized
    case Initialized
    // TODO (KR) : Freed
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Custom
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Custom extends ExtractedType {
    val gen: Generic[?]
    override final lazy val typeRepr: TypeRepr = gen.typeRepr
    val termType: String
    val typeType: String
    def roots: NonEmptyList[ExtractedType.ProductLike]
    def renderRoot: String
    override def toString: String = renderRoot
  }

  /** NOT a Terminal */
  sealed trait NotTerminalLike extends ExtractedType.Custom

  /** Only Terminal */
  sealed trait TerminalLike extends ExtractedType.Custom {
    override final val termType: String = "Terminal"
    override def roots: NonEmptyList[ExtractedType.ProductTerminal]
  }

  /** Only NonTerminal */
  sealed trait NonTerminalLike extends ExtractedType.NotTerminalLike {
    override final val termType: String = "NonTerminal"
    override def roots: NonEmptyList[ExtractedType.ProductNonTerminal]
  }

  sealed trait SumLike extends ExtractedType.Custom {
    override final val typeType: String = "Sum"
    def hasSumChildren: Boolean // TODO (KR) : should this be `any sum children` or only `sum non-terminal`? move to different trait?
  }
  sealed trait ProductLike extends ExtractedType.Custom { override final val typeType: String = "Product" }

  ///////  ///////////////////////////////////////////////////////////////

  final class ProductTerminal(
      val gen: ProductGeneric.CaseClassGeneric[?],
  )(
      val build: Expr[BuildTerminal[gen.AType]],
      val regex: ParsedRegex,
  ) extends ExtractedType.ProductLike,
        ExtractedType.TerminalLike {

    override def roots: NonEmptyList[ExtractedType.ProductTerminal] = NonEmptyList.one(this)

    override def renderInline: String = s"ProductTerminal[${typeRepr.showAnsiCode}]"
    override def renderRoot: String =
      s"ProductTerminal[${typeRepr.showAnsiCode}]: ${regex.regexText}"

    // =====|  |=====

    override protected def initializeInternal(parentPos: Position, cache: ExtractedTypeCache)(using Quotes): Unit = ()

  }

  final class ProductNonTerminal(
      val gen: ProductGeneric.CaseClassGeneric[?],
  ) extends ExtractedType.ProductLike,
        ExtractedType.NonTerminalLike {

    final class Field(
        val field: gen.Field[?],
        val extracted: ExtractedType,
    )

    def fields: NonEmptyList[Field] = _fields
    override def roots: NonEmptyList[ExtractedType.ProductNonTerminal] = NonEmptyList.one(this)

    override def renderInline: String = s"ProductNonTerminal[${typeRepr.showAnsiCode}]"
    override def renderRoot: String =
      s"ProductNonTerminal[${typeRepr.showAnsiCode}]:" +
        fields.map { f => s"\n    ${f.field.name}: ${f.extracted.renderInline}" }.mkString

    // =====|  |=====

    private var _fields: NonEmptyList[Field] = null

    override protected def initializeInternal(parentPos: Position, cache: ExtractedTypeCache)(using Quotes): Unit = {
      val rawFields: NonEmptyList[gen.Field[?]] =
        NonEmptyList.fromList(gen.fields.toList).getOrElse { report.errorAndAbort("Not allowed: case class extends NonTerminal with no fields, use `Ignored`", gen.pos) }
      _fields = rawFields.map { field => Field(field, cache.getOrCreate(field.pos)(field.typeRepr)) }
    }

  }

  final class SumTerminal(
      val gen: SumGeneric[?],
  ) extends ExtractedType.SumLike,
        ExtractedType.TerminalLike {

    def directChildren: NonEmptyList[ExtractedType.TerminalLike] = _directChildren
    override def roots: NonEmptyList[ExtractedType.ProductTerminal] = _roots

    override def renderInline: String = s"SumTerminal[${typeRepr.showAnsiCode}]"
    override def renderRoot: String =
      s"SumTerminal[${typeRepr.showAnsiCode}]" + (if hasSumChildren then " (HAS SUM CHILDREN):" else ":") +
        "\n  directChildren:" + directChildren.toList.map { f => s"\n    - ${f.renderInline}" }.mkString +
        "\n  roots:" + roots.toList.map { f => s"\n    - ${f.renderInline}" }.mkString

    override def hasSumChildren: Boolean = _hasSumChildren

    // =====|  |=====

    private var _directChildren: NonEmptyList[ExtractedType.TerminalLike] = null
    private var _roots: NonEmptyList[ExtractedType.ProductTerminal] = null
    private var _hasSumChildren: Boolean = false

    override protected def initializeInternal(parentPos: Position, cache: ExtractedTypeCache)(using Quotes): Unit = {
      val rawCases: NonEmptyList[gen.Case[?]] =
        NonEmptyList.fromList(gen.cases.toList).getOrElse { report.errorAndAbort("Not allowed: sealed trait extends Terminal with no cases", gen.pos) }
      _directChildren = rawCases.map { kase => cache.getOrCreateNarrowed[ExtractedType.TerminalLike](kase.pos)(kase.typeRepr) }.flatMap(_.roots)
      _roots = _directChildren.flatMap(_.roots)
      _hasSumChildren = _directChildren.exists { case _: SumLike => true; case _ => false }
    }

  }

  final class SumNonTerminal(
      val gen: SumGeneric[?],
  ) extends ExtractedType.SumLike,
        ExtractedType.NonTerminalLike {

    def directChildren: NonEmptyList[ExtractedType.NonTerminalLike] = _directChildren
    override def roots: NonEmptyList[ExtractedType.ProductNonTerminal] = _roots

    override def renderInline: String = s"SumNonTerminal[${typeRepr.showAnsiCode}]"
    override def renderRoot: String =
      s"SumNonTerminal[${typeRepr.showAnsiCode}]" + (if hasSumChildren then " (HAS SUM CHILDREN):" else ":") +
        "\n  directChildren:" + directChildren.toList.map { f => s"\n    - ${f.renderInline}" }.mkString +
        "\n  roots:" + roots.toList.map { f => s"\n    - ${f.renderInline}" }.mkString

    override def hasSumChildren: Boolean = _hasSumChildren

    // =====|  |=====

    private var _directChildren: NonEmptyList[ExtractedType.NonTerminalLike] = null
    private var _roots: NonEmptyList[ExtractedType.ProductNonTerminal] = null
    private var _hasSumChildren: Boolean = false

    override protected def initializeInternal(parentPos: Position, cache: ExtractedTypeCache)(using Quotes): Unit = {
      val rawCases: NonEmptyList[gen.Case[?]] =
        NonEmptyList.fromList(gen.cases.toList).getOrElse { report.errorAndAbort("Not allowed: sealed trait extends NonTerminal with no cases", gen.pos) }
      _directChildren = rawCases.map { kase => cache.getOrCreateNarrowed[ExtractedType.NonTerminalLike](kase.pos)(kase.typeRepr) }
      _roots = _directChildren.flatMap(_.roots)
      _hasSumChildren = _directChildren.exists { case _: SumLike => true; case _ => false }
    }

  }

  final class SumElement(
      val gen: SumGeneric[?],
  ) extends ExtractedType.SumLike,
        ExtractedType.NotTerminalLike {

    override val termType: String = "Element"

    def directChildren: NonEmptyList[ExtractedType.Custom] = _directChildren
    def nonTerminalRoots: NonEmptyList[ExtractedType.ProductNonTerminal] = _nonTerminalRoots
    def terminalRoots: NonEmptyList[ExtractedType.ProductTerminal] = _terminalRoots
    override def roots: NonEmptyList[ExtractedType.ProductLike] = nonTerminalRoots ++ terminalRoots

    override def renderInline: String = s"SumElement[${typeRepr.showAnsiCode}]"
    override def renderRoot: String =
      s"SumElement[${typeRepr.showAnsiCode}]" + (if hasSumChildren then " (HAS SUM CHILDREN):" else ":") +
        "\n  directChildren:" + directChildren.toList.map { f => s"\n    - ${f.renderInline}" }.mkString +
        "\n  roots:" + roots.toList.map { f => s"\n    - ${f.renderInline}" }.mkString

    override def hasSumChildren: Boolean = _hasSumChildren

    // =====|  |=====

    private var _directChildren: NonEmptyList[ExtractedType.Custom] = null
    private var _terminalRoots: NonEmptyList[ExtractedType.ProductTerminal] = null
    private var _nonTerminalRoots: NonEmptyList[ExtractedType.ProductNonTerminal] = null
    private var _hasSumChildren: Boolean = false

    override protected def initializeInternal(parentPos: Position, cache: ExtractedTypeCache)(using Quotes): Unit = {
      val rawCases: NonEmptyList[gen.Case[?]] =
        NonEmptyList.fromList(gen.cases.toList).getOrElse { report.errorAndAbort("Not allowed: sealed trait extends Element with no cases", gen.pos) }
      _directChildren = rawCases.map { kase => cache.getOrCreateNarrowed[ExtractedType.Custom](kase.pos)(kase.typeRepr) }
      val allRoots: List[ExtractedType.ProductLike] = _directChildren.toList.flatMap(_.roots.toList)
      _terminalRoots = NonEmptyList
        .fromList(allRoots.collect { case t: ExtractedType.ProductTerminal => t })
        .getOrElse { report.errorAndAbort("extend NonTerminal, you don't have any Terminal children" + allRoots.map { r => s"\n  - ${r.typeRepr.showAnsiCode}" }.mkString, gen.pos) }
      _nonTerminalRoots = NonEmptyList
        .fromList(allRoots.collect { case t: ExtractedType.ProductNonTerminal => t })
        .getOrElse { report.errorAndAbort("extend Terminal, you don't have any NonTerminal children" + allRoots.map { r => s"\n  - ${r.typeRepr.showAnsiCode}" }.mkString, gen.pos) }
      _hasSumChildren = _directChildren.exists { case _: SumLike => true; case _ => false }
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      BuiltIn
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait BuiltIn extends ExtractedType

  final case class IgnoreBuiltIn(typeRepr: TypeRepr) extends BuiltIn {
    override protected def initializeInternal(parentPos: Position, cache: ExtractedTypeCache)(using Quotes): Unit = ()

    override def renderInline: String = "Ignored"

  }

  final class UnionBuiltIn(
      val typeRepr: TypeRepr,
  ) extends ExtractedType.BuiltIn {

    def cases: NonEmptyList[ExtractedType.Custom] = _cases

    override def renderInline: String = cases.map(_.renderInline).mkString("(  ", "  |  ", "  )")

    // =====|  |=====

    private var _cases: NonEmptyList[ExtractedType.Custom] = null

    override protected def initializeInternal(parentPos: Position, cache: ExtractedTypeCache)(using Quotes): Unit =
      _cases = NonEmptyList.unsafeFromList(typeRepr.orChildren.toList).map(cache.getOrCreateNarrowed[ExtractedType.Custom](parentPos)(_))

  }

  final class ElementOptionBuiltIn(
      val typeRepr: TypeRepr,
      val elemTypeRepr: TypeRepr,
  ) extends ExtractedType.BuiltIn {

    def elem: ExtractedType = _elem

    override def renderInline: String = s"ElementOption[  ${elem.renderInline}  ]"

    // =====|  |=====

    private var _elem: ExtractedType = null

    override protected def initializeInternal(parentPos: Position, cache: ExtractedTypeCache)(using Quotes): Unit =
      _elem = cache.getOrCreate(parentPos)(elemTypeRepr)

  }

  final class ElementListBuiltIn(
      val typeRepr: TypeRepr,
      val elemTypeRepr: TypeRepr,
  ) extends ExtractedType.BuiltIn {

    def elem: ExtractedType = _elem

    override def renderInline: String = s"ElementList[  ${elem.renderInline}  ]"

    // =====|  |=====

    private var _elem: ExtractedType = null

    override protected def initializeInternal(parentPos: Position, cache: ExtractedTypeCache)(using Quotes): Unit =
      _elem = cache.getOrCreate(parentPos)(elemTypeRepr)

  }

  final class NonEmptyElementListBuiltIn(
      val typeRepr: TypeRepr,
      val elemTypeRepr: TypeRepr,
  ) extends ExtractedType.BuiltIn {

    def elem: ExtractedType = _elem

    override def renderInline: String = s"NonEmptyElementList[  ${elem.renderInline}  ]"

    // =====|  |=====

    private var _elem: ExtractedType = null

    override protected def initializeInternal(parentPos: Position, cache: ExtractedTypeCache)(using Quotes): Unit =
      _elem = cache.getOrCreate(parentPos)(elemTypeRepr)

  }

  final class VElementListBuiltIn(
      val typeRepr: TypeRepr,
      val elemTypeRepr: TypeRepr,
      val beforeTypeRepr: TypeRepr,
      val betweenTypeRepr: TypeRepr,
      val afterTypeRepr: TypeRepr,
  ) extends ExtractedType.BuiltIn {

    def elem: ExtractedType = _elem
    def before: ExtractedType = _before
    def between: ExtractedType = _between
    def after: ExtractedType = _after

    override def renderInline: String = s"VElementList[  ${elem.renderInline},  ${before.renderInline},  ${between.renderInline},  ${after.renderInline}  ]"

    // =====|  |=====

    private var _elem: ExtractedType = null
    private var _before: ExtractedType = null
    private var _between: ExtractedType = null
    private var _after: ExtractedType = null

    override protected def initializeInternal(parentPos: Position, cache: ExtractedTypeCache)(using Quotes): Unit = {
      _elem = cache.getOrCreate(parentPos)(elemTypeRepr)
      _before = cache.getOrCreate(parentPos)(beforeTypeRepr)
      _between = cache.getOrCreate(parentPos)(betweenTypeRepr)
      _after = cache.getOrCreate(parentPos)(afterTypeRepr)
    }

  }

  final class NonEmptyVElementListBuiltIn(
      val typeRepr: TypeRepr,
      val elemTypeRepr: TypeRepr,
      val beforeTypeRepr: TypeRepr,
      val betweenTypeRepr: TypeRepr,
      val afterTypeRepr: TypeRepr,
  ) extends ExtractedType.BuiltIn {

    def elem: ExtractedType = _elem
    def before: ExtractedType = _before
    def between: ExtractedType = _between
    def after: ExtractedType = _after

    override def renderInline: String = s"NonEmptyVElementList[  ${elem.renderInline},  ${before.renderInline},  ${between.renderInline},  ${after.renderInline}  ]"

    // =====|  |=====

    private var _elem: ExtractedType = null
    private var _before: ExtractedType = null
    private var _between: ExtractedType = null
    private var _after: ExtractedType = null

    override protected def initializeInternal(parentPos: Position, cache: ExtractedTypeCache)(using Quotes): Unit = {
      _elem = cache.getOrCreate(parentPos)(elemTypeRepr)
      _before = cache.getOrCreate(parentPos)(beforeTypeRepr)
      _between = cache.getOrCreate(parentPos)(betweenTypeRepr)
      _after = cache.getOrCreate(parentPos)(afterTypeRepr)
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      doExtract
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def doExtractSealed(parentPos: Position, cache: ExtractedTypeCache)(typeRepr: TypeRepr)(using Quotes): ExtractedType.SumLike = {
    type T
    given Type[T] = typeRepr.asTypeOf
    val gen: SumGeneric[T] = SumGeneric.of[T](Derivable.Config(defaultUnrollStrategy = SumGeneric.UnrollStrategy.Nested))
    cache.elementType(parentPos)(gen.typeRepr) match {
      case ElementType.Element     => new SumElement(gen)
      case ElementType.Terminal    => new SumTerminal(gen)
      case ElementType.NonTerminal => new SumNonTerminal(gen)
    }
  }

  private def doExtractCaseClass(parentPos: Position, cache: ExtractedTypeCache)(typeRepr: TypeRepr)(using Quotes): ExtractedType.ProductLike = {
    type T
    given Type[T] = typeRepr.asTypeOf
    val gen: ProductGeneric.CaseClassGeneric[T] = ProductGeneric.CaseClassGeneric.of[T]
    cache.elementType(parentPos)(gen.typeRepr) match {
      case ElementType.Element     => report.errorAndAbort(s"[${gen.typeRepr.showAnsiCode}] case class must extend Terminal or NonTerminal", gen.pos)
      case ElementType.NonTerminal => new ProductNonTerminal(gen)
      case ElementType.Terminal    =>
        val build: Expr[BuildTerminal[gen.AType]] =
          Implicits.searchOption[BuildTerminal[gen.AType]].getOrElse { DeriveBuildTerminal.derivedImpl[gen.AType](parentPos, true, gen) }
        val regAnnot: Expr[regex] =
          gen.annotations.optionalOf[regex].getOrElse { report.errorAndAbort("Missing required `@regex(\"...\".r)` annotation", gen.pos) }
        val annotPos: Position = regAnnot.toTerm.pos

        val regString: String = regAnnot match
          case '{ new `regex`((${ Expr(string) }: String).r) } => string
          case _                                               => report.errorAndAbort("Unable to extract regex", regAnnot)
        val parsedRegex: RegularExpression = RegularExpression.parse(Source(regString, None)) match
          case Right(value) => value
          case Left(error)  => report.errorAndAbort(s"Unable to parse regex for ${gen.typeRepr.showAnsiCode}\n$error", regAnnot)
        new ProductTerminal(gen)(build, ParsedRegex(regString, parsedRegex, annotPos))
    }
  }

  def doExtract(parentPos: Position, cache: ExtractedTypeCache)(rawTpe: TypeRepr)(using Quotes): ExtractedType = {
    def fail(msg: String): Nothing = report.errorAndAbort(msg, parentPos)

    val typeRepr: TypeRepr = rawTpe.dealiasKeepOpaques

    (typeRepr, typeRepr.asType) match {

      /////// BuiltIn ///////////////////////////////////////////////////////////////
      case (typeRepr: OrType, _) =>
        new UnionBuiltIn(typeRepr)
      case (_, '[ElementOption[elem]]) =>
        new ElementOptionBuiltIn(typeRepr, TypeRepr.of[elem])
      case (_, '[ElementList[elem]]) =>
        new ElementListBuiltIn(typeRepr, TypeRepr.of[elem])
      case (_, '[NonEmptyElementList[elem]]) =>
        new NonEmptyElementListBuiltIn(typeRepr, TypeRepr.of[elem])
      case (_, '[VElementList[elem, before, between, after]]) =>
        new VElementListBuiltIn(typeRepr, TypeRepr.of[elem], TypeRepr.of[before], TypeRepr.of[between], TypeRepr.of[after])
      case (_, '[NonEmptyVElementList[elem, before, between, after]]) =>
        new NonEmptyVElementListBuiltIn(typeRepr, TypeRepr.of[elem], TypeRepr.of[before], TypeRepr.of[between], TypeRepr.of[after])

      /////// Explicitly Reject ///////////////////////////////////////////////////////////////
      case (_: AndType, _)         => fail(s"AndType not supported : ${typeRepr.showAnsiCode}")
      case (_, '[Option[a]])       => fail("Use ElementOption instead of Option (slyce.core.builtIn.*)")
      case (_, '[List[a]])         => fail("Use ElementList/VElementList instead of List (slyce.core.builtIn.*)")
      case (_, '[NonEmptyList[a]]) => fail("Use NonEmptyElementList/NonEmptyVElementList instead of NonEmptyList (slyce.core.builtIn.*)")

      /////// Custom ///////////////////////////////////////////////////////////////
      case _ =>
        typeRepr.typeType.option match {
          case Some(value) =>
            value match {
              case _: TypeType.Sealed      => doExtractSealed(parentPos, cache)(typeRepr)
              case _: TypeType.Case.Class  => doExtractCaseClass(parentPos, cache)(typeRepr)
              case _: TypeType.Case.Object => fail(s"Unable to derive parser for case object : ${typeRepr.showAnsiCode}")
            }
          case None => fail(s"Unknown type : ${typeRepr.showAnsiCode}")
        }

    }
  }

}
