package slyce.generate.grammar

import java.util.regex.Pattern
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.collection.mutable
import scala.quoted.*

import slyce.generate.*

/** Early surface checks that **grammar rewrite cannot fix**.
  *
  * Adjacent FIRST-overlap on products (list vs trailer, etc.) is handled by [[GrammarRewrite]] + LR table construction — not aborted here.
  *
  * Still hard-fails sum alternatives whose **distinct leading terminals** can match the same input (lexer ambiguity), e.g. overlapping DomainLabel / Ipv4Octet regexes before a letter-start split.
  *
  * FIRST sets are computed with a fixed-point so left-recursive products (`TypeSelect(lhs: TypeExpr, …) extends TypeExpr`) do not stack-overflow.
  */
private[slyce] object GrammarValidity {

  def assertValid(root: ExtractedType, cache: ExtractedTypeCache)(using Quotes): Unit = {
    val rootName = root.typeRepr.showCode
    val first = FirstTable(cache)
    cache.getAllTypes.foreach {
      case s: ExtractedType.SumNonTerminal => checkSum(rootName, s.directChildren.toList, s.typeRepr.showCode, first)
      case s: ExtractedType.SumElement     => checkSum(rootName, s.directChildren.toList, s.typeRepr.showCode, first)
      case s: ExtractedType.SumTerminal    => checkSum(rootName, s.directChildren.toList, s.typeRepr.showCode, first)
      case _                               => ()
    }
  }

  private def checkSum(
      rootName: String,
      children: List[ExtractedType],
      sumName: String,
      first: FirstTable,
  )(using Quotes): Unit = {
    val leads: List[(ExtractedType, Set[ExtractedType.ProductTerminal])] =
      children.map(c => c -> first.terminals(c))

    leads.combinations(2).foreach {
      case List((c1, t1), (c2, t2)) =>
        val cross = for {
          a <- t1
          b <- t2
          if a.typeRepr.showCode != b.typeRepr.showCode
          // For an equal-length overlap THIS CHECK DETECTS, an EXPLICIT priority difference makes the tie
          // deterministic (see `slyce.parse.priority` and `ParserImpl.lexOne`), so such overlaps are ALLOWED
          // — this is what lets a higher-priority keyword terminal coexist with an `Identifier` regex.
          // Overlaps with EQUAL priority (the default 0 included) remain a hard error. Caveat: the overlap
          // check is a finite-sample heuristic (see `regexesCanBothMatch`), so a genuine equal-priority
          // equal-length collision it fails to sample slips through and falls back to first-match-wins at
          // runtime (Set-order-dependent; pre-existing, not introduced by priority).
          if a.regex.priority == b.regex.priority
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

  /** Fixed-point FIRST sets over the extracted type graph (handles left recursion). */
  private final class FirstTable(cache: ExtractedTypeCache) {
    private val terms = mutable.Map.empty[ExtractedType.TypeId, Set[ExtractedType.ProductTerminal]]
    private val nullable = mutable.Map.empty[ExtractedType.TypeId, Boolean]

    cache.getAllTypes.foreach { t =>
      terms(t.typeId) = Set.empty
      nullable(t.typeId) = false
    }

    private def seed(et: ExtractedType): Unit =
      et match {
        case t: ExtractedType.ProductTerminal =>
          terms(t.typeId) = Set(t)
          nullable(t.typeId) = false
        case t: ExtractedType.SumTerminal =>
          terms(t.typeId) = t.roots.toList.toSet
          nullable(t.typeId) = false
        case _: ExtractedType.IgnoreBuiltIn =>
          nullable(et.typeId) = true
        case _: ExtractedType.ElementListBuiltIn | _: ExtractedType.ElementOptionBuiltIn =>
          nullable(et.typeId) = true // may refine in iteration
        case _ => ()
      }

    cache.getAllTypes.foreach(seed)

    private def step(et: ExtractedType): Boolean = {
      val (newTerms, newNull) =
        et match {
          case t: ExtractedType.ProductTerminal =>
            (Set(t: ExtractedType.ProductTerminal), false)
          case t: ExtractedType.ProductNonTerminal =>
            var acc = Set.empty[ExtractedType.ProductTerminal]
            var stillNull = true
            t.fields.toList.foreach { f =>
              if stillNull then {
                acc ++= terms.getOrElse(f.extracted.typeId, Set.empty)
                stillNull = nullable.getOrElse(f.extracted.typeId, false)
              }
            }
            (acc, stillNull)
          case t: ExtractedType.SumNonTerminal =>
            val kids = t.directChildren.toList
            (kids.flatMap(c => terms.getOrElse(c.typeId, Set.empty)).toSet, kids.exists(c => nullable.getOrElse(c.typeId, false)))
          case t: ExtractedType.SumTerminal =>
            (t.roots.toList.toSet, false)
          case t: ExtractedType.SumElement =>
            val kids = t.directChildren.toList
            (kids.flatMap(c => terms.getOrElse(c.typeId, Set.empty)).toSet, kids.exists(c => nullable.getOrElse(c.typeId, false)))
          case t: ExtractedType.ElementListBuiltIn =>
            (terms.getOrElse(t.elem.typeId, Set.empty), true)
          case t: ExtractedType.NonEmptyElementListBuiltIn =>
            (terms.getOrElse(t.elem.typeId, Set.empty), nullable.getOrElse(t.elem.typeId, false))
          case t: ExtractedType.ElementOptionBuiltIn =>
            (terms.getOrElse(t.elem.typeId, Set.empty), true)
          case _: ExtractedType.IgnoreBuiltIn =>
            (Set.empty, true)
          case t: ExtractedType.UnionBuiltIn =>
            val kids = t.cases.toList
            (kids.flatMap(c => terms.getOrElse(c.typeId, Set.empty)).toSet, kids.exists(c => nullable.getOrElse(c.typeId, false)))
          case t: ExtractedType.VElementListBuiltIn =>
            // before + elem contribute; treat as nullable only if empty list possible — V list can be empty
            val beforeT = terms.getOrElse(t.before.typeId, Set.empty)
            val elemT = terms.getOrElse(t.elem.typeId, Set.empty)
            val beforeN = nullable.getOrElse(t.before.typeId, false)
            (if beforeN then beforeT ++ elemT else beforeT, true)
          case t: ExtractedType.NonEmptyVElementListBuiltIn =>
            val beforeT = terms.getOrElse(t.before.typeId, Set.empty)
            val elemT = terms.getOrElse(t.elem.typeId, Set.empty)
            val beforeN = nullable.getOrElse(t.before.typeId, false)
            (if beforeN then beforeT ++ elemT else beforeT, false)
          case _ =>
            (Set.empty[ExtractedType.ProductTerminal], false)
        }

      val oldT = terms.getOrElse(et.typeId, Set.empty)
      val oldN = nullable.getOrElse(et.typeId, false)
      if newTerms != oldT || newNull != oldN then {
        terms(et.typeId) = newTerms
        nullable(et.typeId) = newNull
        true
      } else false
    }

    var changed = true
    var guard = 0
    while changed && guard < 10_000 do {
      guard += 1
      changed = false
      cache.getAllTypes.foreach { t =>
        if step(t) then changed = true
      }
    }

    def terminals(et: ExtractedType): Set[ExtractedType.ProductTerminal] =
      terms.getOrElse(et.typeId, Set.empty)
  }

  /** Decide whether two terminal regexes can BOTH match some common input at the SAME LENGTH.
    *
    * Only EQUAL-LENGTH overlaps matter here. The runtime lexer (`ParserImpl.lexOne`) resolves DIFFERENT-length overlaps deterministically by maximal munch (longest match wins), so a mere prefix
    * overlap — e.g. `in` vs `insert`, or `struct` vs an `Identifier` that starts with it — is NOT an ambiguity and must NOT be flagged. Only when two terminals can match the SAME input to the SAME
    * end position is the lexer forced into a genuine tie, which priority (`ParserImpl.lexOne` / `slyce.parse.priority`) is designed to break. So this returns true only when some sample string is
    * matched by BOTH regexes ending at the same position (`ma.end() == mb.end()`, both `lookingAt` from index 0, non-empty).
    *
    * SOUNDNESS GAP (documented, not fully closed): this is a heuristic that probes a FINITE sample of candidate strings, not a real regex-intersection-emptiness test. A genuine equal-length overlap
    * that only occurs on a string OUTSIDE the sample is missed here, so an ambiguous equal-priority grammar can still slip past this check; at runtime that unflagged tie then falls back to
    * first-match-wins (dependent on lexer `allowed`-set / Set iteration order — a pre-existing property, unchanged by priority). To make the common keyword-vs-identifier case reliable, the two
    * patterns' OWN texts (`pa`, `pb`) are added to the sample: for a literal-keyword regex like `struct` this puts the exact keyword string into the probe, so its equal-length overlap with an
    * `Identifier` regex is detected (see the `findings-hard-keywords` analysis). A complete fix would be a genuine regex-intersection check.
    */
  private def regexesCanBothMatch(pa: String, pb: String): Boolean = {
    val ca =
      try Pattern.compile(pa)
      catch { case _: Exception => return false }
    val cb =
      try Pattern.compile(pb)
      catch { case _: Exception => return false }

    val samples =
      List(pa, pb) ++
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
      // EQUAL-LENGTH common match only: both must match from index 0 and end at the SAME position.
      // Different-length (prefix) overlaps are resolved by maximal munch at runtime and are NOT ambiguous.
      ma.lookingAt() && mb.lookingAt() && ma.end() > 0 && ma.end() == mb.end()
    }
  }

}
