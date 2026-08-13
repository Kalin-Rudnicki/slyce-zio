# Grammar Auto-Rewrite — Findings for Implementers

> **Audience:** Future agent / engineer implementing automatic grammar rewriting for
> slyce-v2 `Parser.derived`.
>
> **Raw research + citations:** `GRAMMAR-RESEARCH-RAW.md`  
> **Human brief:** `GRAMMAR-RESEARCH.html`

---

## 1. The claim you may remember

> “Any LALR(*n*) can be rewritten as LALR(1).”

**Correct, careful version:**

| Claim | Status |
|-------|--------|
| Every **LR(*k*) language** (*k*≥1) is an **LR(1) language** (≡ DCFL) | **True** (Knuth 1965) |
| Every **LR(*k*) grammar** can be **mechanically transformed** into an **equivalent LR(1) grammar** | **True** (Mickunas, Lancaster, Schneider 1976) |
| The transformed grammar has the **same parse trees / AST shape** | **False** — structure changes; need a **cover** or tree map |
| Every **LALR(*k*) grammar** freely becomes **LALR(1)** with the same nonterminals | **Not the standard theorem** — LALR is a *merged-state* restriction; prefer “→ some LR(1) grammar” or “→ LALR(1) grammar for the same language after rewrite” |
| Ambiguous grammars become deterministic by rewrite alone | **False** — need precedence/policy or multi-parse (GLR) |

**Practical takeaway:** Rewrite is real and classical, but it produces a **different grammar G′**. Slyce must parse with G′ and **map** to the user ADT (like `cleaned` + `fromCleaned` today).

---

## 2. What industry tools actually do

Most tools **do not** implement full Mickunas transforms.

| Approach | Who | When to use in slyce |
|----------|-----|----------------------|
| Hard fail on conflicts | slyce today (`GrammarValidity`) | Default until rewrite exists; good for RED tests |
| Default shift/reduce policy | yacc/bison | Avoid silent picks for typed macros |
| Precedence / associativity | yacc, Happy, ANTLR | Expr-like models with annotations |
| Full LR(1) tables | Menhir | Clears **LALR-merge-only** conflicts |
| IELR (split LALR states) | Bison | Same power, smaller than full LR(1) |
| `%inline` expansion | Menhir | Soft factoring without conceptual rewrite |
| Direct left-rec elimination + tree repair | ANTLR4 | Pattern for “hide rewrite from user” (LL world) |
| Hand left-factor / layered NTs | Everyone’s docs | Heuristic targets for auto-rewrite |
| GLR | Bison | Out of scope for linear LALR product unless chosen |

**Implication:** Before Mickunas-scale work, try **stronger table construction** (LR(1)/IELR). Only rewrite grammar when the surface CFG is not LR(1) *as written*.

---

## 3. Architecture to implement (non-negotiable shape)

```
User ADT model  ──extract──►  Surface grammar G
                               │
                    conflict detect (LALR / LR1)
                               │
              ┌────────────────┼────────────────┐
              │ ok             │ LALR-only      │ not LR(1) / pattern
              ▼                ▼                ▼
           codegen         IELR/LR1/clone    rewrite → G′ + cover φ
              │                │                │
              └────────────────┴────────────────┘
                               │
                               ▼
                    tables + reduce fns
                               │
                    runtime: tokens → (G or G′) values
                               │
                               ▼
                         User ADT  (via identity or φ / fromCleaned)
```

**Do not** hardcode domain types (`Url`, `PathAndTrail`) into `FromExtractedType`.  
**Do** match **grammar shapes** (list+optional trailer, shared First sets, etc.).

Aligns with PLAN: human AST → LALR → temp tree → desired tree; oxygen.quoted / oxygen.meta.

---

## 4. Conflict classification (implement this first)

When tables conflict, classify:

1. **LALR-inadequacy** — vanishes under canonical LR(1) / IELR  
   → fix construction or clone nonterminals; **no AST map**.

2. **Classic ambiguity** — expr ops, dangling else  
   → require `@prec` / `@left` / `@right` or layered ADT; document policy.

3. **Insufficient structure / LR(*k*) *k*>1** — reduce decided too early  
   → rewrite (Mickunas-lite or heuristics); **need cover**.

4. **Optional trailer / common prefix** — list then `Option` sharing First  
   → absorb into one parsing NT; cover back to product fields.

5. **True multi-parse or non-DCFL**  
   → hard error with Menhir-quality explanation.

**Decidability warning:** “∃ *k* . G is LR(*k*)” is undecidable. Bound *k* and heuristics; never infinite search in the macro.

---

## 5. Rewrite techniques worth implementing (priority order)

### P0 — Detect + explain
- Keep / improve `GrammarValidity` messages (grammar terms, not only state ids).
- Report whether LR(1) construction would also fail (if affordable).

### P1 — Automaton power
- Optional LR(1) or IELR-like state splitting for LALR-only conflicts.
- Nonterminal cloning as explicit rewrite that preserves AST constructors.

### P2 — High-value local rewrites (heuristic)

**H1. List + optional trailer absorb**  
Shape: fields `(List[A], Option[B])` where `First(B) ⊆ First(A)` or overlapping.  
Parse NT: single sequence that threads trailer.  
Cover: split values back into list + option.

**H2. Left factor common prefixes on one NT**  
Shape: alts of same sum type share terminal/NT prefix.  
Standard Dragon-Book left factor; cover rebuilds the sum.

**H3. Shared-prefix competing NTs**  
Shape: two product types reduce the same prefix tokens; distinction is right context  
(e.g. host label vs octet).  
Introduce synthetic prefix NT; delay commit (Mickunas “premature scan” lite).  
Cover: choose final ADT when suffix known.

**H4. Precedence annotations**  
Shape: recursive expr ADT.  
Either layered NTs or conflict resolution table like yacc.

### P3 — Full Mickunas (only if needed)
Phases from paper:
1. Right-stratification (split long RHS)
2. Right-context extraction
3. Premature scanning
4. Build surjection φ : P′ → P (or → AST builders)

Expect size growth; cap and fail loudly.

---

## 6. Cover / tree mapping (required API)

Every rewrite step must emit **cover actions**, not only new productions.

Suggested cover ops:

| Op | Meaning |
|----|---------|
| `Id` | New prod builds same partial as old |
| `Sequence` | Concat children |
| `Inject(sumCtor)` | Wrap as ADT case |
| `Project(i)` | Drop scaffolding NTs |
| `SplitListTrailer` | `(as :+ last?)` → `(List, Option)` |
| `ChooseBySuffix` | Disambiguate Domain vs IPv4-style |
| `Compose(φ1, φ2)` | Stack rewrites |

Runtime: reduce of G′ builds either:
- **cleaned ADT** then `fromCleaned`, or  
- **user ADT directly** via composed cover.

ANTLR precedent: user must not need to know G′ existed.

---

## 7. What *not* to do

1. **Silent yacc defaults** (always shift) without annotations — wrong for a typed derive macro.
2. **URL-special cases** in core rewrite engine.
3. **Assume language equivalence ⇒ AST equivalence.**
4. **Unbounded rewrite expansion** in compile-time macros.
5. **Treat left-recursion elimination as a goal** — LR *likes* left recursion for lists/exprs.
6. **Confuse lexer merges with grammar rewrites** — both valid; prefer lexer for multi-char ops (`:=`), grammar for structural *k*>1.

---

## 8. Validation plan

| Case | Expectation |
|------|-------------|
| Calculator | No rewrite; still green |
| JSON | No rewrite; still green |
| URL cleaned + hand `fromCleaned` | Stay green (baseline) |
| URL **model** `Parser.derived` after auto-rewrite | Green **without** domain hardcoding |
| Intentional ambiguity without annotations | Still **compile error** |
| Synthetic LR(2) fixture | Auto → LR(1) G′ + cover round-trips AST |
| Growth cap | Macro error if \|P′\| or states explode |

---

## 9. Suggested implementation slices

1. **Conflict class probe** — LALR fail vs LR(1) fail (diagnostic only).  
2. **NT clone rewrite** — fix pure merge issues; cover = same ctor.  
3. **H1 list+trailer absorb** — unblocks many path-like models.  
4. **H2 left factor** on extracted sum types.  
5. **H3 delayed commit** for competing prefixes (host-like).  
6. **Cover codegen** in `DeriveParser` / reduce kinds (general, not URL).  
7. Only then consider full Mickunas or user-facing `@rewrite` knobs.

---

## 10. Key references (read these)

1. **Mickunas et al. 1976** — the LR(*k*)→LR(1) transform + cover story.  
   https://www.cs.sfu.ca/~anoop/courses/CMPT-379-Spring-2004/lrk_xform.pdf  
2. **Knuth 1965** — LR(*k*) languages = DCFL = LR(1) languages.  
3. **Gray & Harrison** — grammatical covers.  
4. **Denny & Malloy IELR** — state splitting without grammar rewrite.  
5. **ANTLR ALL(*) paper §2.2** — rewrite + restore user trees.  
6. **Menhir** — LR(1) + `%inline` + conflict explanations.  
7. Full dump: `GRAMMAR-RESEARCH-RAW.md`.

---

## 11. One-paragraph north star

Slyce should keep deriving parsers from human ADTs. When the extracted grammar is not
LALR(1), first try more precise automata (LR(1)/IELR). If the grammar itself is only
LR(*k*) or poorly factored, apply **shape-driven rewrites** that produce an LALR/LR(1)
grammar G′ plus a **compositional cover** into the original ADT—the same architecture as
hand-written `cleaned` + `fromCleaned`, but general. Theory guarantees an LR(1) grammar
exists for any DCFL; it does **not** guarantee a small rewrite or an identical tree without
an explicit mapping. Fail loud on true ambiguity; never hardcode domain models into the
macro.
