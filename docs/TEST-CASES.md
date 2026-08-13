# Slyce v2 — Test cases & use cases

Notes from planning (not an approved implementation plan).

---

## Round 1 — Progressive technical cases

Internal pipeline / complexity ladder (feature coverage), not product languages.

1. **Single literal terminal** — e.g. `42` → `IntLit`  
   Product terminal, one regex, accept; no lists/sums.

2. **Single product nonterminal** — e.g. `x := 1` → `Assign`  
   Product NT fields, terminal sequence, mixed terminal/nonterminal children.

3. **Sum terminal / sum element choice** — e.g. `true` / `3.14` / `"hi"` → `Literal`  
   Sum roots, multiple regexes, `BuildTerminal` decode, sum discrimination.

4. **Optional / empty list** — empty vs non-empty `ElementList` in `Program`  
   `ElementList` / `ElementNil`, empty production, start-NT choice.

5. **Nested structure (parens)** — `(1)`, `((x))` → `Wrap`  
   Recursive NT, push/reduce of parens, span join.

6. **Ambiguous single char (per-state lex)** — `-` as `AddOp` vs start of `IntLit`  
   State-scoped terminal sets; longest-match among valid only.

7. **One precedence level (left assoc)** — `1+2+3`  
   `AssocNT` or layered NTs, reduce order.

8. **Two precedence levels** — `1+2*3`, `(1+2)*3`  
   Multi-level precedence, mixed ops, parens override.

9. **List with separators (VElementList)** — delimited lists / string parts  
   Lift lists, repeat prods, empty vs non-empty delimited lists.

10. **Full mini-program** — multi-assign + mixed expr + whitespace/end-to-end  
    Full `Program`-shaped integration, ignore/whitespace, `maxLookAhead` if needed.

### Ladder summary

| # | Focus | Unblocks confidence for… |
|---|--------|---------------------------|
| 1–2 | Terminal + product NT | FromExtractedType, reduce → case class |
| 3–4 | Sums + lists | expansion, empty prods |
| 5 | Recursion | table + stack |
| 6 | Per-state lex | lexer design |
| 7–8 | Assoc / precedence | AssocNT, conflict-free tables |
| 9 | Delimited lists | VElementList / LiftList |
| 10 | Real program | full pipeline + whitespace |

---

## Round 2 — Real-world use cases (suite candidates)

Product-shaped languages; golden inputs + fixed subsets as a regression suite.

1. **JSON** (subset → full) — objects, arrays, strings, numbers, bool, null  
   Nested structure, string escapes, lists, sum values.

2. **Dotenv / simple config** — `KEY=value`, comments, optional quotes  
   Line-oriented, ignore/whitespace, optional quoting.

3. **SemVer / version constraints** — `1.2.3`, `^1.2`, `>=1.0 <2`, `||`  
   Overlapping number/op tokens, range combinators.

4. **Cron expression** — 5-field cron with `*`, lists, ranges, steps  
   Dense terminals, fixed multi-field shape.

5. **URL / URI (practical subset)** — scheme, host, path, query, fragment  
   Path lists, optional query/fragment, punctuation-heavy lexing.

6. **Arithmetic / boolean expression language** — ops, comparisons, `&& ||`, unary `-`  
   Precedence/assoc, unary vs binary `-`, look-ahead.

7. **SQL SELECT subset** — SELECT/FROM/WHERE/JOIN/GROUP/ORDER/LIMIT  
   Keywords vs idents, optional clauses, join lists, nested exprs.

8. **GraphQL query document (read-only subset)** — operations, fields, args, variables  
   Nested selection sets, optional args, `$` variables.

9. **Markdown-ish block + inline subset** — headings, paragraphs, code, bold, links  
   Line-sensitive structure, competing inline lexers (stretch).

10. **Mini programming language / infra DSL** — e.g. `let`/`fn`/`if` or block config  
    Statement lists, nested blocks, full integrated parse.

### Suite role (round 2)

| # | Use case | Role in suite |
|---|----------|----------------|
| 1 | JSON | golden nested data |
| 2 | dotenv | line-oriented / ignore |
| 3 | SemVer ranges | small picky tokens |
| 4 | cron | fixed multi-field |
| 5 | URL | punctuation / optionals |
| 6 | expressions | precedence + unary `-` |
| 7 | SQL SELECT | large optional-clause grammar |
| 8 | GraphQL | nested API queries |
| 9 | Markdown subset | semi-structured text (stretch) |
| 10 | mini lang / infra DSL | full language demo |

---

## Chosen use cases to model

The four languages we actually want to model (for design + eventual test suite):

1. **JSON**
2. **URL**
3. **Simple calculator program** — `List[Assignment]` (assignments + expressions; calculator-style program)
4. **BASH**

These supersede “pick any of the ten” for concrete modeling work. Round 1 remains useful as an internal feature checklist; round 2 remains a broader menu if the suite grows later.
