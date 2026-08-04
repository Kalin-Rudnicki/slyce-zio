package slyce.generate.grammar

import java.util.regex.Pattern
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

import slyce.generate.*

/**
 * Early surface checks that **grammar rewrite cannot fix**.
 *
 * Adjacent FIRST-overlap on products (list vs trailer, etc.) is handled by
 * [[GrammarRewrite]] + LR table construction — not aborted here.
 *
 * Still hard-fails sum alternatives whose **distinct leading terminals** can match the
 * same input (lexer ambiguity), e.g. overlapping DomainLabel / Ipv4Octet regexes before
 * a letter-start split.
 */
private[slyce] object GrammarValidity {

  def assertValid(root: ExtractedType, cache: ExtractedTypeCache)(using Quotes): Unit = {
    val rootName = root.typeRepr.showCode
    cache.getAllTypes.foreach {
      case s: ExtractedType.SumNonTerminal => checkSum(rootName, s.directChildren.toList, s.typeRepr.showCode)
      case s: ExtractedType.SumElement     => checkSum(rootName, s.directChildren.toList, s.typeRepr.showCode)
      case s: ExtractedType.SumTerminal    => checkSum(rootName, s.directChildren.toList, s.typeRepr.showCode)
      case _                               => ()
    }
  }

  /** Sum alternatives whose leading terminals can match the same input (e.g. DomainLabel vs Ipv4Octet). */
  private def checkSum(rootName: String, children: List[ExtractedType], sumName: String)(using Quotes): Unit = {
    val leads: List[(ExtractedType, Set[ExtractedType.ProductTerminal])] =
      children.map(c => c -> firstTerminals(c))

    leads.combinations(2).foreach {
      case List((c1, t1), (c2, t2)) =>
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

  private def firstTerminals(et: ExtractedType): Set[ExtractedType.ProductTerminal] =
    firstSet(et)._1

  /** @return (leading product terminals, nullable) */
  private def firstSet(et: ExtractedType): (Set[ExtractedType.ProductTerminal], Boolean) =
    et match {
      case t: ExtractedType.ProductTerminal =>
        (Set(t), false)
      case t: ExtractedType.ProductNonTerminal =>
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
        (e, true)
      case t: ExtractedType.NonEmptyElementListBuiltIn =>
        val (e, en) = firstSet(t.elem)
        (e, en)
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
