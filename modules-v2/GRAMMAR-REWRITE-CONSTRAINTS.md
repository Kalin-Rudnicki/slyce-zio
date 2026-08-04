# Grammar rewrite constraints (no shape hardcoding)

> Design constraints for auto-rewrite in `Parser.derived`.  
> Complements `GRAMMAR-RESEARCH-RAW.md` / `GRAMMAR-RESEARCH-FINDINGS.md`.  
> **Status:** binding for implementers — not optional style.

---

## 1. Hard rule

**Do not hardcode Scala AST / type shapes** as the rewrite trigger.

Forbidden patterns (non-exhaustive):

| Forbidden | Why |
|-----------|-----|
| Look for `ElementList` then `ElementOption` adjacent fields | Misses other shapes; Ex2 hides it behind products |
| Look for `ElementList` then `ElementList` only as a special case | Same class of problem as list+option; must fall out of grammar analysis |
| Hardcode `Url`, `PathSeg`, `PathAndTrail`, field names `path` / `trailingSlash` | Domain coupling; rejected once already |
| Any rewrite that imports or names test/model packages | Macro core must not know fixtures |

**Allowed:** operate on the **expanded grammar** (`GSym`, productions, FIRST/FOLLOW, LALR/LR conflicts, nullability). Covers map G′ → user ADT without domain knowledge.

---

## 2. Motivating counterexamples

These must be treated as the **same kind of problem** at the grammar level. A field-shape detector fails on both.

### Ex1 — no `Option` at all

```scala
// Two consecutive lists of the same (or FIRST-overlapping) item type — no Option involved.
final case class Ex1(a: ElementList[T], b: ElementList[T])
```

After extraction, both lists are nullable / continuable on the same lead terminals.  
There is **no** list-then-option pair. A “list+option absorb” special case **never fires**, yet the grammar is still bad.

### Ex2 — list and option exist, but not as adjacent fields of one product

```scala
final case class Ex2A(a: ElementList[T])
final case class Ex2B(b: ElementOption[T])
final case class Ex2(a: Ex2A, b: Ex2B)
```

Shallow product walk on `Ex2` only sees:

```text
Ex2 → Ex2A  Ex2B
```

The list and option are **nested** inside intermediate nonterminals. After expansion:

```text
Ex2  → Ex2A Ex2B
Ex2A → List[T]     (roughly)
Ex2B → Opt[T]
```

The conflict lives on **symbols / FIRST / nullability in the CFG**, not on “adjacent fields named like path + trail.”

### Url path (fixture, not a special case)

```scala
// model.Url fields (simplified)
path: ElementList[PathSeg]      // PathSeg leads with `/`
trailingSlash: ElementOption[`/`]
```

Same **grammar** phenomenon as Ex1/Ex2 (continuation vs next field on shared FIRST).  
Domain labels already require a letter start so host is **not** part of this rewrite story.

---

## 3. What detection must look like

| Approach | Ex1 | Ex2 | Url path+trail |
|----------|-----|-----|----------------|
| Hardcode list+option product fields | miss | miss (nested) | hit (lucky only) |
| Hardcode type / field names | miss | miss | hit only |
| Analyze **expanded grammar** (FIRST, nullable, conflicts) | same class | same class | same class |

### Pipeline (required shape)

```text
ExtractedType
    → FromExtractedType  (normal products / lists / opts — no rewrite yet)
    → Grammar G  (GSym, productions)
    → Analyze G  (FIRST/FOLLOW, LALR/LR conflicts, adequacy)
    → Either:
         rewrite G → G′ + compositional cover φ
         or hard compile error (explain in grammar terms)
    → Codegen tables + reduces for G′ / φ → user ADT
```

Notes:

- Surface checks on `ExtractedType` field pairs (like today’s path-specific `GrammarValidity` messaging) are **at best** early hints. They are **Ex2-blind** and must not become the rewrite engine.
- Source of truth for “is this LALR-safe / what to rewrite” is the **CFG** (and/or table construction), including through intermediate NTs.

---

## 4. What rewrite may look like

Rewrites are **grammar operations**, for example:

- factor common prefixes on productions
- absorb / delay reduces when right context is required
- clone nonterminals when LALR merge is the only issue
- introduce synthetic NTs with **generated** names, not domain names

Each step must produce a **compositional cover** (reduce of G′ → original ADT values). No `fromCleaned`-style host/path logic inside the macro.

---

## 5. Non-goals / failure modes to reject in review

1. **PathAndTrail 2.0** — any NT or reduce kind named after a domain concept.  
2. **“Just handle list+option”** as the whole feature — even if phrased as a “heuristic,” if the detector is `List` field next to `Option` field, it is hardcoding.  
3. **Silent yacc defaults** to “make Url green.”  
4. **Url-only green** without Ex1/Ex2-style fixtures proving the same rule path.

---

## 6. Validation expectations

When rewrite lands, tests should include at least:

1. **Non-URL fixture** with Ex1-shaped consecutive same-FIRST lists (or overlapping FIRST).  
2. **Non-URL fixture** with Ex2-shaped nesting (list and option only inside intermediate products).  
3. **Url model** as a *consumer* of the general rule (path + trailing slash), not the driver of special cases.  
4. Calculator / JSON unchanged (no accidental rewrite).  
5. True ambiguity without a general rule still **hard fails**.

---

## 7. One-paragraph north star

Auto-rewrite must classify and transform **grammars**, not Scala field layouts. Ex1 and Ex2 exist specifically to forbid detectors that only notice “list then option on one product.” Url’s path/trailer case is the same conflict class after expansion; host was removed from this story by requiring domain labels to start with a letter. Implement conflict-driven (or FIRST-driven) rewrites on the production graph with compositional covers — or fail closed. No domain hardcoding, no list+option field hardcoding.

---

## 8. Related docs

| Doc | Role |
|-----|------|
| `GRAMMAR-RESEARCH-RAW.md` | Theory, tools, Mickunas, covers, citations |
| `GRAMMAR-RESEARCH-FINDINGS.md` | Agent-oriented summary (update if it still over-emphasizes “H1 list+option fields”) |
| `GRAMMAR-RESEARCH.html` | Human brief |
| This file | **Binding constraints** against shape/domain hardcoding |
