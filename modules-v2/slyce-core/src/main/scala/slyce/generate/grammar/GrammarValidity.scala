package slyce.generate.grammar

import java.util.regex.Pattern
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

import slyce.generate.*

/**
 * Surface-grammar validity checks **before** / independent of full LR construction.
 *
 * Contract: if the programmer-facing AST implies an ambiguous or otherwise non-LALR-safe
 * surface grammar (without rewrite), derivation must **fail at compile time** — not emit a
 * silent wrong parser.
 *
 * Auto-rewrite is future work; until then, these cases are hard errors.
 */
private[slyce] object GrammarValidity {

  def assertValid(root: ExtractedType, cache: ExtractedTypeCache)(using Quotes): Unit = {
    val rootName = root.typeRepr.showCode
    cache.getAllTypes.foreach {
      case p: ExtractedType.ProductNonTerminal => checkProduct(rootName, p)
      case s: ExtractedType.SumNonTerminal     => checkSum(rootName, s.directChildren.toList, s.typeRepr.showCode)
      case s: ExtractedType.SumElement         => checkSum(rootName, s.directChildren.toList, s.typeRepr.showCode)
      case s: ExtractedType.SumTerminal        => checkSum(rootName, s.directChildren.toList, s.typeRepr.showCode)
      case _                                   => ()
    }
  }

  private def checkProduct(rootName: String, p: ExtractedType.ProductNonTerminal)(using Quotes): Unit = {
    val fields = p.fields.toList.map(f => (f.field.name, f.extracted))
    fields.sliding(2).foreach {
      case List((lName, leftEt), (rName, rightEt)) =>
        checkAdjacentFields(rootName, p.typeRepr.showCode, lName, leftEt, rName, rightEt)
      case _ => ()
    }
  }

  /**
   * Classic ambiguity we hit on URL:
   *   path: ElementList[PathSeg]   // PathSeg starts with `/`
   *   trailingSlash: ElementOption[`/`]
   * After zero or more path segs, `/` can start another PathSeg **or** the trailing slash.
   */
  private def checkAdjacentFields(
      rootName: String,
      productName: String,
      leftName: String,
      leftEt: ExtractedType,
      rightName: String,
      rightEt: ExtractedType,
  )(using Quotes): Unit = {
    val leftIsList =
      leftEt.isInstanceOf[ExtractedType.ElementListBuiltIn] ||
        leftEt.isInstanceOf[ExtractedType.NonEmptyElementListBuiltIn]

    if leftIsList then {
      val elemFirst = firstTerminals(elemOfList(leftEt))
      val (rightFirst, _) = firstSet(rightEt)
      val overlap = overlappingTerminals(elemFirst, rightFirst)
      if overlap.nonEmpty then
        report.errorAndAbort(
          s"""|Invalid grammar for $rootName (while checking $productName):
              |  Ambiguous consecutive fields — FIRST sets overlap (not LALR-safe without rewrite):
              |    $leftName: ${leftEt.renderInline}
              |    $rightName: ${rightEt.renderInline}
              |  Overlapping terminal(s): ${overlap.map(_.typeRepr.showCode).mkString(", ")}
              |  A token in that set can continue the list **or** start the next field.
              |  Auto-rewrite is not implemented yet; fix the AST or parse a cleaned grammar + transform.
              |""".stripMargin,
        )
    }

    // nullable left + overlapping FIRST with right (general FIRST/FOLLOW hazard)
    val (leftFirst, leftNullable) = firstSet(leftEt)
    val (rightFirst, _) = firstSet(rightEt)
    if leftNullable then {
      val overlap = overlappingTerminals(leftFirst, rightFirst)
      if overlap.nonEmpty && !leftIsList then // list case already reported more specifically
        report.errorAndAbort(
          s"""|Invalid grammar for $rootName (while checking $productName):
              |  Ambiguous nullable field followed by overlapping FIRST:
              |    $leftName: ${leftEt.renderInline}
              |    $rightName: ${rightEt.renderInline}
              |  Overlapping terminal(s): ${overlap.map(_.typeRepr.showCode).mkString(", ")}
              |""".stripMargin,
        )
    }
  }

  /** Sum alternatives whose leading terminals can match the same input (e.g. DomainLabel vs Ipv4Octet). */
  private def checkSum(rootName: String, children: List[ExtractedType], sumName: String)(using Quotes): Unit = {
    val leads: List[(ExtractedType, Set[ExtractedType.ProductTerminal])] =
      children.map(c => c -> firstTerminals(c))

    leads.combinations(2).foreach {
      case List((c1, t1), (c2, t2)) =>
        val overlap = overlappingTerminals(t1, t2)
        // same terminal type appearing in both is fine (same symbol); distinct terms with regex overlap is not
        val cross = for {
          a <- t1
          b <- t2
          if a.typeRepr.showCode != b.typeRepr.showCode
          if regexesCanBothMatch(a.regex.regexText, b.regex.regexText)
        } yield (a, b)
        if cross.nonEmpty then {
          val pairs = cross.map { case (a, b) => s"${a.typeRepr.showCode} ~ ${b.typeRepr.showCode}" }.mkString(", ")
          report.errorAndAbort(
            s"""|Invalid grammar for $rootName (while checking sum $sumName):
                |  Ambiguous alternatives — distinct leading terminals can match the same input:
                |    ${c1.renderInline}
                |    ${c2.renderInline}
                |  Conflicting terminal pair(s): $pairs
                |  Lexer cannot uniquely choose a token; this is not a valid surface grammar without rewrite/disambiguation.
                |""".stripMargin,
          )
        }
      case _ => ()
    }
  }

  private def elemOfList(et: ExtractedType): ExtractedType =
    et match {
      case e: ExtractedType.ElementListBuiltIn          => e.elem
      case e: ExtractedType.NonEmptyElementListBuiltIn  => e.elem
      case _                                            => et
    }

  private def firstTerminals(et: ExtractedType): Set[ExtractedType.ProductTerminal] =
    firstSet(et)._1

  /** @return (leading product terminals, nullable) */
  private def firstSet(et: ExtractedType): (Set[ExtractedType.ProductTerminal], Boolean) =
    et match {
      case t: ExtractedType.ProductTerminal =>
        (Set(t), false)
      case t: ExtractedType.ProductNonTerminal =>
        // FIRST of product = FIRST of fields until a non-nullable field
        var terms = Set.empty[ExtractedType.ProductTerminal]
        var nullable = true
        t.fields.toList.foreach { f =>
          if nullable then {
            val (ft, fn) = firstSet(f.extracted)
            terms ++= ft
            nullable = fn
          }
        }
        (terms, nullable)
      case t: ExtractedType.SumNonTerminal =>
        val parts = t.directChildren.toList.map(firstSet)
        (parts.flatMap(_._1).toSet, parts.exists(_._2))
      case t: ExtractedType.SumTerminal =>
        (t.roots.toList.toSet, false)
      case t: ExtractedType.SumElement =>
        val parts = t.directChildren.toList.map(firstSet)
        (parts.flatMap(_._1).toSet, parts.exists(_._2))
      case t: ExtractedType.ElementListBuiltIn =>
        val (e, _) = firstSet(t.elem)
        (e, true) // empty list
      case t: ExtractedType.NonEmptyElementListBuiltIn =>
        val (e, en) = firstSet(t.elem)
        (e, en) // only nullable if elem is
      case t: ExtractedType.ElementOptionBuiltIn =>
        val (e, _) = firstSet(t.elem)
        (e, true)
      case _: ExtractedType.IgnoreBuiltIn =>
        (Set.empty, true)
      case t: ExtractedType.UnionBuiltIn =>
        val parts = t.cases.toList.map(firstSet)
        (parts.flatMap(_._1).toSet, parts.exists(_._2))
      case _ =>
        (Set.empty, false)
    }

  private def overlappingTerminals(
      a: Set[ExtractedType.ProductTerminal],
      b: Set[ExtractedType.ProductTerminal],
  ): Set[ExtractedType.ProductTerminal] = {
    // same terminal type in both sets
    val byNameA = a.map(t => t.typeRepr.showCode -> t).toMap
    val byNameB = b.map(t => t.typeRepr.showCode -> t).toMap
    val same = byNameA.keySet.intersect(byNameB.keySet).flatMap(byNameA.get)
    // distinct terminals with regex overlap
    val cross =
      for {
        x <- a
        y <- b
        if x.typeRepr.showCode != y.typeRepr.showCode
        if regexesCanBothMatch(x.regex.regexText, y.regex.regexText)
      } yield x
    same ++ cross
  }

  /** True if there exists some non-empty string that both patterns can match as a prefix (lookingAt). */
  private def regexesCanBothMatch(pa: String, pb: String): Boolean = {
    val ca =
      try Pattern.compile(pa)
      catch { case _: Exception => return false }
    val cb =
      try Pattern.compile(pb)
      catch { case _: Exception => return false }

    val samples =
      List(
        "0",
        "1",
        "12",
        "127",
        "255",
        "256",
        "a",
        "A",
        "com",
        "example",
        "x1",
        "1x",
        "/",
        "://",
        ".",
        "?",
        "#",
        "=",
        "&",
        ":",
        "-",
        "_",
        " ",
      ) ++
        (0 to 30).map(_.toString) ++
        ('a' to 'z').map(_.toString)

    samples.exists { s =>
      val ma = ca.matcher(s)
      val mb = cb.matcher(s)
      ma.lookingAt() && mb.lookingAt() && ma.end() > 0 && mb.end() > 0
    }
  }

}
