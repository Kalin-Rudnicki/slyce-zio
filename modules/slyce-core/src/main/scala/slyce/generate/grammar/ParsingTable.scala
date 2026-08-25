package slyce.generate.grammar

import oxygen.predef.core.*
import scala.annotation.tailrec

import slyce.generate.Helpers

/** LR parse table with nested look-ahead (ported/cleaned from v1 ParsingTable). Symbols are GSym; no stringly grammar DSL.
  */
private[slyce] final case class ParsingTable(
    states: List[ParsingTable.ParseState],
)
private[slyce] object ParsingTable {

  final case class ParseState(
      id: Int,
      goto: Map[GSym.NonTerm, Int],
      lookAhead: Action,
  )

  enum Action {
    case Accept
    case Reduce(nt: GSym.NonTerm, prodIdx: Int)
    case Shift(toState: Int)
    case LookAhead(
        onTerm: Map[GSym.Term, Action],
        onEOF: Option[Action], // Accept | Reduce only
    )
  }

  def fromExpandedGrammar(grammar: ExpandedGrammar): Either[String, ParsingTable] = {
    if grammar.maxLookAhead < 1 then return Left("maxLookAhead must be >= 1")

    val raw = grammar.rawNTs
    val defined: Map[GSym.NonTerm, List[Closure.Production]] =
      raw.map { nt =>
        val prods = nt.productions.toList.zipWithIndex.map { case (p, idx) =>
          Closure.Production(ReducesTo.Production(nt.name, idx), p.elements)
        }
        nt.name -> prods
      }.toMap

    val referenced: Set[GSym.NonTerm] =
      raw.flatMap(_.productions.toList.flatMap(_.elements)).collect { case nt: GSym.NonTerm => nt }.toSet

    val missing = referenced -- defined.keySet
    if missing.nonEmpty then return Left(s"Undefined NTs: ${missing.map(_.label).mkString(", ")}")
    if !defined.contains(grammar.startNt) then return Left(s"Start NT not defined: ${grammar.startNt.label}")

    val initialEntry =
      Closure.Entry(
        reducesTo = ReducesTo.Start,
        seen = Nil,
        waiting = grammar.startNt :: Nil,
        lookAhead = LA.eofOnly,
      )
    val initialClosure = expandEntries(defined, Set(initialEntry), grammar.maxLookAhead)

    val allClosures: List[Closure] =
      Helpers
        .findAll(Set(initialClosure)) { c =>
          calcTransitionMap(defined, c, grammar.maxLookAhead).values.toSet
        }
        .toList

    val pairs: Either[String, List[(Closure, TmpState)]] =
      allClosures.foldLeft[Either[String, List[(Closure, TmpState)]]](Right(Nil)) { (acc, c) =>
        acc.flatMap { list =>
          calcActionState(defined, c, grammar.maxLookAhead).map(s => (c, s) :: list)
        }
      }

    pairs.map { ps =>
      val map = ps.toMap
      val initial = map(initialClosure)
      val others = map.values.toSet - initial
      val stateList = (initial :: others.toList).zipWithIndex
      val actionToId = stateList.toMap
      val closureToId = map.map { case (c, as) => c -> actionToId(as) }

      val states = stateList.map { case (as, id) =>
        ParseState(
          id = id,
          goto = as.goto.map { case (nt, toC) => nt -> closureToId(toC) },
          lookAhead = convertLookAhead(closureToId, as.lookAhead),
        )
      }
      ParsingTable(states)
    }
  }

  // =====| Internal types |=====

  private enum ReducesTo {
    case Start
    case Production(nt: GSym.NonTerm, idx: Int)
  }

  /** A set of look-ahead strings (each up to `maxLookAhead` terminals long), represented as a PREFIX TREE.
    *
    * `byTerm(t)` holds the continuations that can follow terminal `t`; `eof` marks that a string may end here (end-of-input is a valid follow at this point). Keeping continuations per-branch (rather
    * than a flat set-per-position) preserves the correlation BETWEEN positions: e.g. `W` followed only by `{),EOF}` stays distinct from `)` followed by `{P,…}`, so the merge never fabricates the
    * phantom string `W P`. The old set-per-position representation unioned each depth independently and admitted such phantom cross-products, which made recursive-operator + ignore-slot grammars fail
    * to converge at any finite k.
    *
    * A node with empty `byTerm` and `eof == false` is a leaf: a look-ahead string ends here (either because it was truncated at `maxLookAhead`, or a recursion cutoff contributed nothing further).
    */
  private final case class LA(byTerm: Map[GSym.Term, LA], eof: Boolean) {
    def isEmpty: Boolean = byTerm.isEmpty && !eof
  }
  private object LA {
    val empty: LA = LA(Map.empty, eof = false)
    val eofOnly: LA = LA(Map.empty, eof = true)
    def single(t: GSym.Term, next: LA): LA = LA(Map(t -> next), eof = false)
  }

  private def unionLA(a: LA, b: LA): LA =
    if a.byTerm.isEmpty then LA(b.byTerm, a.eof || b.eof)
    else if b.byTerm.isEmpty then LA(a.byTerm, a.eof || b.eof)
    else {
      val merged =
        (a.byTerm.keySet ++ b.byTerm.keySet).iterator.map { t =>
          val v =
            (a.byTerm.get(t), b.byTerm.get(t)) match {
              case (Some(x), Some(y)) => unionLA(x, y)
              case (Some(x), None)    => x
              case (None, Some(y))    => y
              case (None, None)       => LA.empty // unreachable: keys come from the keySet union
            }
          t -> v
        }.toMap
      LA(merged, a.eof || b.eof)
    }

  private def unionAll(las: IterableOnce[LA]): LA =
    las.iterator.foldLeft(LA.empty)(unionLA)

  /** Truncate a look-ahead trie to at most `depth` terminals deep. */
  private def truncateLA(la: LA, depth: Int): LA =
    if depth <= 0 then LA.empty
    else if la.byTerm.isEmpty then la
    else LA(la.byTerm.map { case (t, n) => t -> truncateLA(n, depth - 1) }, la.eof)

  private final case class Closure(entries: Set[Closure.Entry]) {
    lazy val finished: Set[Closure.Entry.Finished] = entries.collect { case e: Closure.Entry.Finished => e }
    lazy val unfinished: Set[Closure.Entry.Waiting] = entries.collect { case e: Closure.Entry.Waiting => e }
  }
  private object Closure {
    final case class Production(reducesTo: ReducesTo.Production, produces: List[GSym])

    sealed trait Entry {
      def reducesTo: ReducesTo
      def seen: List[GSym]
      def lookAhead: LA
      def waitingList: List[GSym]
    }
    object Entry {
      final case class Finished(reducesTo: ReducesTo, seen: List[GSym], lookAhead: LA) extends Entry {
        override def waitingList: List[GSym] = Nil
      }
      final case class Waiting(reducesTo: ReducesTo, seen: List[GSym], waiting: NonEmptyList[GSym], lookAhead: LA) extends Entry {
        override def waitingList: List[GSym] = waiting.toList
      }

      def apply(reducesTo: ReducesTo, seen: List[GSym], waiting: List[GSym], lookAhead: LA): Entry =
        NonEmptyList.fromList(waiting) match {
          case Some(w) => Waiting(reducesTo, seen, w, lookAhead)
          case None    => Finished(reducesTo, seen, lookAhead)
        }
    }
  }

  private final case class TmpState(
      goto: Map[GSym.NonTerm, Closure],
      lookAhead: TmpAction,
  )

  private enum TmpAction {
    case Accept
    case Reduce(prod: Closure.Production)
    case Shift(to: Closure)
    case LookAhead(onTerm: Map[GSym.Term, TmpAction], onEOF: Option[TmpAction])
  }

  // =====| Algorithm (v1-style, cleaned) |=====

  /** FIRST_k of the symbol sequence `ids`, falling through to `ifPassThrough` when `ids` is exhausted, as a prefix tree of look-ahead strings. NonTerminals are inlined (guarded by `alreadyExpanded`,
    * a set of `(production, position)` keys, reset each time a terminal is consumed) to avoid infinite recursion.
    */
  private def calcLookAhead(
      productionsForNT: Map[GSym.NonTerm, List[Closure.Production]],
      ids: List[(ReducesTo, Int, GSym)],
      alreadyExpanded: Set[(ReducesTo, Int)],
      ifPassThrough: LA,
      maxLookAhead: Int,
  ): LA =
    if maxLookAhead <= 0 then LA.empty
    else
      ids match {
        case Nil                  => truncateLA(ifPassThrough, maxLookAhead)
        case (rt, sc, id) :: tail =>
          id match {
            case nt: GSym.NonTerm =>
              if alreadyExpanded.contains((rt, sc)) then LA.empty
              else {
                val newExpanded = alreadyExpanded + (rt -> sc)
                val inlined: List[List[(ReducesTo, Int, GSym)]] =
                  productionsForNT.getOrElse(nt, Nil).map { case Closure.Production(prt, waiting) =>
                    waiting.zipWithIndex.map { case (sym, idx) => (prt: ReducesTo, idx, sym) }
                  }
                unionAll(inlined.map(more => calcLookAhead(productionsForNT, more ::: tail, newExpanded, ifPassThrough, maxLookAhead)))
              }
            case t: GSym.Term =>
              LA.single(t, calcLookAhead(productionsForNT, tail, Set.empty, ifPassThrough, maxLookAhead - 1))
          }
      }

  private def expandEntries(
      productionsForNT: Map[GSym.NonTerm, List[Closure.Production]],
      initial: Set[Closure.Entry],
      maxLookAhead: Int,
  ): Closure = {
    val preJoined =
      Helpers.findAll(initial) {
        case Closure.Entry.Waiting(rt, seen, NonEmptyList(next: GSym.NonTerm, waiting), lookAhead) =>
          val lookup = productionsForNT.getOrElse(next, Nil)
          val newIds = waiting.zipWithIndex.map { case (id, idx) => (rt, seen.size + 1 + idx, id) }
          val newFollows = calcLookAhead(productionsForNT, newIds, Set.empty, lookAhead, maxLookAhead)
          lookup.map { case Closure.Production(prod, waitingElems) =>
            Closure.Entry(prod, Nil, waitingElems, newFollows)
          }.toSet
        case _ => Set.empty
      }

    val joined =
      preJoined
        .groupMap(e => (e.reducesTo, e.seen, e.waitingList))(_.lookAhead)
        .toSet
        .map { case ((rt, seen, waiting), follows) =>
          Closure.Entry(rt, seen, waiting, unionAll(follows))
        }

    Closure(joined)
  }

  private def calcTransitionMap(
      productionsForNT: Map[GSym.NonTerm, List[Closure.Production]],
      closure: Closure,
      maxLookAhead: Int,
  ): Map[GSym, Closure] =
    closure.unfinished
      .map { case Closure.Entry.Waiting(rt, seen, NonEmptyList(next, stillWaiting), lookAhead) =>
        (next, Closure.Entry(rt, seen :+ next, stillWaiting, lookAhead))
      }
      .groupMap(_._1)(_._2)
      .map { case (id, entries) => id -> expandEntries(productionsForNT, entries, maxLookAhead) }

  private def calcFollowsForClosure(
      productionsForNT: Map[GSym.NonTerm, List[Closure.Production]],
      c: Closure,
      maxLookAhead: Int,
  ): LA =
    unionAll(
      c.entries.toList.map { e =>
        calcLookAhead(
          productionsForNT,
          e.waitingList.zipWithIndex.map { case (id, idx) => (e.reducesTo, e.seen.size + idx, id) },
          Set.empty,
          e.lookAhead,
          maxLookAhead,
        )
      },
    )

  private def calcActionState(
      productionsForNT: Map[GSym.NonTerm, List[Closure.Production]],
      closure: Closure,
      maxLookAhead: Int,
  ): Either[String, TmpState] = {
    val transitions = calcTransitionMap(productionsForNT, closure, maxLookAhead)
    val (ntList, tList) =
      transitions.partitionMap {
        case (nt: GSym.NonTerm, c) => Left(nt -> c)
        case (t: GSym.Term, c)     => Right(t -> (c, calcFollowsForClosure(productionsForNT, c, maxLookAhead)))
      }
    val termMap = tList.toMap
    val res = calcTerminalActions(termMap, closure.finished, Nil)
    // Opt-in conflict diagnostics: the one-line "Need more look-ahead" error names the path + productions
    // but not WHY. With SLYCE_DEBUG set, dump the whole conflicting state — every reduce item with its
    // look-ahead strings, plus the shift terminals and in-progress items — which is what actually lets you
    // tell a genuine LR(k) need apart from an over-approximation. Zero cost when the env var is unset.
    if res.isLeft && sys.env.contains("SLYCE_DEBUG") then {
      // How deep into each reduce item's look-ahead trie the dump prints before eliding with "…".
      val debugLADepth = 6
      def showLA(la: LA, depth: Int): String =
        if depth <= 0 then "…"
        else {
          val terms = la.byTerm.toList.map { case (t, n) => s"${t.label}->${showLA(n, depth - 1)}" }
          (terms ++ (if la.eof then List("EOF") else Nil)).mkString("{", ", ", "}")
        }
      System.err.println(s"slyce conflict-state dump: ${res.swap.getOrElse("")}")
      System.err.println(s"  shift terminals: ${termMap.keys.map(_.label).toList.sorted}")
      closure.finished.foreach { e =>
        System.err.println(s"  REDUCE ${e.reducesTo}/${e.seen.size}  seen=[${e.seen.map(_.label).mkString(" ")}]  LA=${showLA(e.lookAhead, debugLADepth)}")
      }
      closure.unfinished.foreach { e =>
        System.err.println(s"  ITEM ${e.reducesTo}: [${e.seen.map(_.label).mkString(" ")}] . [${e.waitingList.map(_.label).mkString(" ")}]")
      }
    }
    res.map { la =>
      TmpState(ntList.toMap, la)
    }
  }

  private def calcTerminalActions(
      terminalTransitionMap: Map[GSym.Term, (Closure, LA)],
      finishedEntries: Set[Closure.Entry.Finished],
      rFollowedPath: List[GSym.Term],
  ): Either[String, TmpAction] = {
    // `fesWithout`: reduce/accept items whose look-ahead string ran out at this descent point (a genuine
    // "need more than k" conflict). `fesActive`: those with at least one more terminal edge or an EOF end.
    val (fesWithout, fesActive) =
      finishedEntries.partitionMap(e => if e.lookAhead.isEmpty then Left(e) else Right(e))

    if fesWithout.nonEmpty then {
      val path = rFollowedPath.reverse.map(_.label).mkString(", ")
      val conflicts = fesWithout.map(fe => s"${fe.reducesTo}/${fe.seen.size}").mkString(", ")
      Left(s"Need more look-ahead (path: $path; conflicts: $conflicts). Try increasing maxLookAhead.")
    } else if fesActive.isEmpty then {
      val onTerm = terminalTransitionMap.map { case (t, (c, _)) => t -> TmpAction.Shift(c) }
      Right(TmpAction.LookAhead(onTerm, None))
    } else {
      // Reduce items indexed by their next terminal, each ADVANCED past that terminal (its continuation trie).
      val fesByTerm: Map[GSym.Term, Set[Closure.Entry.Finished]] =
        fesActive.toList
          .flatMap { e => e.lookAhead.byTerm.toList.map { case (t, next) => t -> Closure.Entry.Finished(e.reducesTo, e.seen, next) } }
          .groupMap(_._1)(_._2)
          .view
          .mapValues(_.toSet)
          .toMap

      val fesEOF: Set[Closure.Entry.Finished] = fesActive.filter(_.lookAhead.eof)

      val onEOF: Either[String, Option[TmpAction]] =
        fesEOF.toList match {
          case Nil          => Right(None)
          case value :: Nil =>
            value.reducesTo match {
              case ReducesTo.Start          => Right(Some(TmpAction.Accept))
              case rt: ReducesTo.Production => Right(Some(TmpAction.Reduce(Closure.Production(rt, value.seen))))
            }
          case values => Left(s"Multiple EOF actions: ${values.map(v => s"${v.reducesTo}/${v.seen.size}")}")
        }

      val onTermPart: Either[String, Map[GSym.Term, TmpAction]] =
        fesByTerm.toList
          .foldLeft[Either[String, List[(GSym.Term, TmpAction)]]](Right(Nil)) { case (acc, (t, fes)) =>
            acc.flatMap { list =>
              (fes.toList, terminalTransitionMap.get(t)) match {
                case (Nil, Some((c, _))) =>
                  Right((t -> TmpAction.Shift(c)) :: list)
                case (fe :: Nil, None) =>
                  fe.reducesTo match {
                    case prod: ReducesTo.Production =>
                      Right((t -> TmpAction.Reduce(Closure.Production(prod, fe.seen))) :: list)
                    case ReducesTo.Start =>
                      Left("Unexpected reduce-to-start on terminal")
                  }
                case (fes2, None) =>
                  calcTerminalActions(Map.empty, fes2.toSet, t :: rFollowedPath).map(a => (t -> a) :: list)
                case (fes2, Some((c, closureLA))) =>
                  if closureLA.isEmpty then Left(s"No more look-ahead for closure on ${t.label}")
                  else {
                    val descended = closureLA.byTerm.map { case (tt, cont) => tt -> (c, cont) }
                    calcTerminalActions(descended, fes2.toSet, t :: rFollowedPath).map(a => (t -> a) :: list)
                  }
              }
            }
          }
          .map(_.toMap)

      for {
        partial1 <- onTermPart
        eof <- onEOF
      } yield {
        val referenced = fesActive.flatMap(_.lookAhead.byTerm.keySet)
        val partial2 =
          terminalTransitionMap.iterator
            .filterNot { case (t, _) => referenced.contains(t) }
            .map { case (t, (c, _)) => t -> TmpAction.Shift(c) }
            .toMap
        TmpAction.LookAhead(partial1 ++ partial2, eof)
      }
    }
  }

  private def convertLookAhead(closureToId: Map[Closure, Int], action: TmpAction): Action =
    action match {
      case TmpAction.Accept              => Action.Accept
      case TmpAction.Reduce(prod)        => Action.Reduce(prod.reducesTo.nt, prod.reducesTo.idx)
      case TmpAction.Shift(to)           => Action.Shift(closureToId(to))
      case TmpAction.LookAhead(onT, onE) =>
        Action.LookAhead(
          onT.map { case (t, a) => t -> convertLookAhead(closureToId, a) },
          onE.map(convertLookAhead(closureToId, _)),
        )
    }

}
