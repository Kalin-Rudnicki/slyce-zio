# Slyce v2 — Implementation Plan

> Macro-derived parser generator. Replaces v1's external code-gen tool with compile-time `Parser.derived[A]`.

**Branch:** `current/refactor/v2-round2`  
**Status:** Type extraction done; grammar generation next.

---

## Current state

| Layer | Status | Notes |
|-------|--------|-------|
| Core types (`Element`, `Terminal`, `NonTerminal`, `Span`, …) | ✅ Done | `slyce/core/` |
| Built-in wrappers (`ElementList`, `ElementOption`, `VElementList`, …) | ✅ Done | `slyce/core/builtIn/` |
| Regex AST + parser | ✅ Done | `slyce/parse/RegularExpression.scala` |
| `BuildTerminal` derivation | ✅ Done | `DeriveBuildTerminal.scala` |
| `ExtractedType` + cache | ✅ Done | Recursively classifies ADT types |
| `ExtractedType` → grammar | ❌ Not started | **Next major work** |
| Grammar expansion / dedup | 🔲 Stubbed | v1 code in `FIX-PRE-MERGE` comments |
| LR parsing table | 🔲 Stubbed | v1 code in `FIX-PRE-MERGE` comments |
| Per-parse-state lexer | ❌ Not in v2 | **Depends on parsing table** — see below |
| Macro codegen (`Expr[Parser[A]]`) | ❌ `???` | `DeriveParser.scala` |

### Debug probe (temporary)

`DeriveParser.derivedImpl` currently calls `report.errorAndAbort` with the extracted type tree.  
Use this pattern at each phase to inspect output before wiring the next layer.

---

## Pipeline

```
ExtractedType     NTGroups      ExpandedGrammar     ParsingTable       Per-state Lexers      Parser[A]
     │                │                 │                  │                    │                │
     │ FromExtracted  │  port v1 dedup  │  port v1 LR      │  NFA/DFA per state │  macro quotes  │
     ▼                ▼                 ▼                  ▼                    ▼                ▼
   DONE ────────► NEW ──────────► port ────────────► port ─────────────► NEW ────────────► NEW
```

### Critical dependency: parse states before lexers

v2 does **not** use v1's "tokenize the whole file, then parse" model.

v1 `Parser` has separate `lexer` + `grammar`; v2 `Parser[A]` is a single `parse(source)` — lexing happens **inside** the parse loop, driven by the current parse state.

Each parse state's `lookAhead.actionsOnTerminals` (built during LR table construction) defines which terminals are valid at that point. The lexer for parse state `N` is a DFA that only matches the regexes for **that state's valid terminals**. This is what resolves overlaps like `-` matching both `AddOp` and `IntLit`: the parse state picks which regexes to try.

**Implication:** the parsing table must be fully generated before any lexer. Phase 4 is not "port the global DFA"; it is "for each `ParseState`, build a lexer from its valid-terminal set."

| | v1 | v2 (intended) |
|---|---|---|
| Lexer scope | One global DFA from lexer DSL | One DFA per parse state |
| Lexer input | All terminal regexes | Subset from `ParseState.lookAhead` |
| When lexing runs | Upfront on full source | On demand at each parse step |
| Disambiguation | Parse-time look-ahead on pre-tokenized stream | Parse state selects which regexes to run |

---

## Open decisions

Record choices here before implementing Phase 2.

- [ ] **Assoc detection:** automatic heuristic vs `@leftAssoc` / `@rightAssoc` annotation?
  - _Default recommendation:_ heuristic + annotation override
- [ ] **NT naming for codegen:** `typeRepr.showCode` vs sanitized name?
  - _Default recommendation:_ `showCode` for named NTs; `ExtractedType.TypeId` for anon list/opt NTs
- [ ] **Error strategy:** fail-fast per error vs aggregate `Validated` errors?
  - _Default recommendation:_ fail-fast during extraction; `Validated` for table conflicts
- [ ] **Look-ahead depth for lexer:** does each parse-state lexer only need first-token terminals, or must it pre-lex `maxLookAhead` tokens?
  - _Default recommendation:_ lexer produces one token; nested `LookAhead` actions handle multi-token disambiguation at parse time (same as v1 table structure)

---

## Phase 0 — Project hygiene

**Goal:** Make v2 easy to iterate on.

- [ ] Add `slyce-core-v2` to `slyce-root.aggregate(...)` in `build.sbt`
- [ ] Port shared types referenced by stubs:
  - [ ] `LiftList.scala` ← `modules/slyce-generate/.../grammar/LiftList.scala`
  - [ ] `ids.scala` (`AnonListNtId`) ← `modules/slyce-generate/.../grammar/ids.scala`
- [ ] Split `DeriveParser` debug helpers:
  - [ ] `renderExtractedTypes(cache)` — current behaviour
  - [ ] `renderExpandedGrammar(grammar)` — phase 2+
  - [ ] `renderParsingTable(table)` — phase 3+

**Done when:** `sbt slyce-core-v2/compile` passes.

---

## Phase 1 — Restore grammar data structures

**Goal:** Uncomment stubbed grammar layer; compile against `ExtractedType`-backed identifiers.

### Files

| File | Action |
|------|--------|
| `generate/grammar/Production.scala` | Uncomment `Production(elements: List[Identifier])` |
| `generate/grammar/Identifier.scala` | Finish design: variants hold `underlyingType: ExtractedType` |
| `generate/grammar/NTGroup.scala` | Uncomment full `enum NTGroup` |
| `generate/grammar/RawNT.scala` | Uncomment + wire `convertNTGroup` |
| `generate/grammar/Expansion.scala` | Uncomment `Expansion` + `mergeNTGroup` |
| `generate/grammar/ExpandedGrammar.scala` | **New** — port container, `removeDuplicates`, `convertNTGroup` from v1 |

### Identifier design (v2)

v1 identifiers were strings. v2 identifiers carry a type reference for macro codegen:

```scala
sealed trait Identifier {
  val underlyingType: ExtractedType
}
// + structural variants: NamedNt, AnonListNt, AnonOptNt, Terminal, Raw, …
```

### Tasks

- [ ] Uncomment / implement files above
- [ ] Port `removeDuplicates` from v1 `ExpandedGrammar.scala`
- [ ] Port `convertNTGroup` from v1 `ExpandedGrammar.scala`
- [ ] Hand-construct a trivial `NTGroup.BasicNT` in a unit test (no macros)

**Done when:** Grammar types compile; manual `NTGroup` test passes.

---

## Phase 2 — `FromExtractedType`

**Goal:** Bridge `ExtractedType` tree → `NTGroup` list. The one genuinely new module.

### New file

`generate/grammar/FromExtractedType.scala`

```scala
object FromExtractedType {
  def apply(
    root: ExtractedType,
    cache: ExtractedTypeCache,
    maxLookAhead: Int,
  ): ExpandedGrammar
}
```

### `expand(et: ExtractedType): Expansion[Identifier]`

| `ExtractedType` | Maps to |
|-----------------|---------|
| `ProductNonTerminal` | `BasicNT` — one production = expanded fields (skip `IgnoreBuiltIn`) |
| `SumNonTerminal` | `BasicNT` — one production per case |
| `SumElement` | `BasicNT` — one production per child root |
| `ProductTerminal` | `Identifier.Terminal` (lexer only, no NTGroup) |
| `ElementOption` | `Optional` wrapping `expand(elem)` |
| `ElementList` | `ListNT(*)` |
| `NonEmptyElementList` | `ListNT(+)` |
| `VElementList` / `NonEmptyVElementList` | `ListNT` with `LiftList(before, elem, after)` + optional `repeatProds` |
| `UnionBuiltIn` | Multiple productions in `BasicNT` |
| `IgnoreBuiltIn` | Omitted from production |

### Entry point

1. Collect all `ProductNonTerminal` / `SumNonTerminal` roots from cache
2. Expand each into `NTGroup`s (mirror v1 `expandNamedNT`)
3. Run `removeDuplicates`
4. Set `startNt` from root type

### Tasks — Phase 2a (simple cases first)

- [ ] `ProductTerminal` → terminal identifier
- [ ] `ProductNonTerminal` → `BasicNT`
- [ ] `SumNonTerminal` → `BasicNT` (one prod per case)
- [ ] `SumElement` → `BasicNT` (one prod per child root)
- [ ] `ElementOption` → `Optional`
- [ ] `ElementList` / `NonEmptyElementList` → `ListNT`
- [ ] `UnionBuiltIn` → multiple productions
- [ ] `IgnoreBuiltIn` → skip
- [ ] Wire `DeriveParser` to dump expanded grammar (temporary)
- [ ] Manually verify `ParserSpec.Program` dump

### Tasks — Phase 2b (defer until table building needs it)

- [ ] `VElementList` / `NonEmptyVElementList` → `ListNT` with `LiftList`
- [ ] `AssocNT` detection for `Bin(lhs, op, rhs: SameNt)` patterns
- [ ] Apply to `Expr.Node1.Bin`, `Expr.Node2.Bin` in `ParserSpec`

**Done when (2a):** `ParserSpec` grammar dump is structurally correct for non-assoc types.  
**Done when (2b):** Precedence/assoc types expand to `AssocNT`; table builds without conflicts.

---

## Phase 3 — Parsing table

**Goal:** `ExpandedGrammar` → LR parse states.

### Files to uncomment / port

| v2 stub | Port from |
|---------|-----------|
| `generate/parse/ReducesTo.scala` | v1 `ParsingTable.scala` (inner types) |
| `generate/parse/Follow.scala` | v1 `ParsingTable.scala` |
| `generate/parse/Closure.scala` | v1 `ParsingTable.scala` |
| `generate/parse/TmpActionState.scala` | v1 `ParsingTable.scala` |
| `generate/parse/ParseState.scala` | v1 `ParsingTable.scala` |
| `generate/ParsingTable.scala` | **New** — v1 `ParsingTable.scala` |

### Tasks

- [ ] Uncomment parse-layer stubs
- [ ] Adapt `Identifier` references to v2 (`underlyingType`)
- [ ] Implement `ParsingTable.fromExpandedGrammar`
- [ ] Wire `DeriveParser` to dump table or conflict errors (temporary)
- [ ] `ParserSpec` table builds cleanly (or actionable conflict messages)

**Done when:** LR table builds for `ParserSpec` without unresolved conflicts.

---

## Phase 4 — Per-parse-state lexer generation

**Goal:** For each `ParseState`, build a DFA that tokenizes only the terminals valid at that state.

**Prerequisite:** Phase 3 complete — full parsing table with all parse states and `lookAhead` actions.

### Port from v1 (building blocks only)

- `modules/slyce-generate/.../lexer/NFA.scala`
- `modules/slyce-generate/.../lexer/DFA.scala`

Adapt to v2 `RegularExpression` AST (already in `ParsedRegex`).  
Do **not** port v1's global `LexerInput` → single DFA pipeline wholesale.

### New file

`generate/lexer/PerStateLexer.scala` (name TBD)

```scala
object PerStateLexer {
  /** Extract the terminal set a parse state can act on (top-level lookAhead keys). */
  def validTerminals(state: ParseState): Set[Identifier.Term]

  /** Build a DFA for only the given terminals' regexes. */
  def build(terminals: Set[Identifier.Term], cache: ExtractedTypeCache): DFA

  /** Map: parseStateId → DFA (or Lexer.State) */
  def forTable(table: ParsingTable, cache: ExtractedTypeCache): Map[Int, DFA]
}
```

### How it connects

1. `ParsingTable.fromExpandedGrammar` produces `List[ParseState]` with nested `LookAhead` actions.
2. For each parse state, collect valid `Identifier.Term` keys from its `lookAhead` tree.
3. Resolve each term to a `ProductTerminal` via `Identifier.underlyingType` → `ParsedRegex`.
4. Build a combined NFA/DFA over just those regexes (priority / longest-match rules TBD).
5. At runtime, `ParserImpl` holds `Map[parseStateId, Lexer.State]` and calls the matching lexer before each shift.

### Tasks

- [ ] Port NFA/DFA core (regex → automaton)
- [ ] `validTerminals(parseState)` — walk `LookAhead` tree, collect term keys
- [ ] `buildDfaForTerminals(terms, cache)` — subset DFA from `ProductTerminal` regexes
- [ ] `forTable(table, cache)` — produce per-state lexer map
- [ ] Handle `SumTerminal` / `SumElement`: multiple `ProductTerminal` roots map to one logical terminal kind
- [ ] Unit test: given a specific parse state + source position, lexer picks the right token
  - e.g. state expecting `Literal` reads `42` as `IntLit`, not `TextIdent`
  - e.g. state expecting `AddOp` reads `-` as `AddOp`, not start of `IntLit`

**Done when:** For a known parse state ID and input snippet, the per-state lexer returns the expected terminal.

---

## Phase 5 — Reduce actions / `Extras`

**Goal:** Connect table reductions → case class construction at macro time.

v1 `Extras.build` maps `NTGroup` variants to tree-building metadata.  
v2 emits `Expr` trees instead of source strings.

### New file

`generate/Extras.scala`

| `NTGroup` variant | Codegen action |
|-----------------|----------------|
| `BasicNT` | `ProductGeneric.instantiate` |
| `SumNonTerminal` | `SumGeneric` case selection |
| `ListNT` | `ElementList` / `NonEmptyElementList` builders |
| `LiftNT` / `VElementList` | `toVList` / `toList` plumbing |
| `Optional` | `ElementOption` / `ElementNil` |
| `AssocNT` | Assoc-specific reduce logic |

### Tasks

- [ ] Port `Extras` logic from v1 `output/Extras.scala`
- [ ] Rewrite output as quoted `Expr` trees
- [ ] Hand-test one reduce path in isolation

**Done when:** A single quoted reduce produces a correctly typed AST node.

---

## Phase 6 — Wire `DeriveParser` + integrated parse loop

**Goal:** Replace `???` and debug aborts with `Expr[Parser[A]]`.

```scala
def derivedImpl[A: Type](maxLookAhead: Expr[Int])(using Quotes): Expr[Parser[A]] = {
  val cache      = ExtractedTypeCache.empty
  val root       = cache.getOrCreate(...)(TypeRepr.of[A])
  val grammar    = FromExtractedType(root, cache, maxLookAhead)
  val table      = ParsingTable.fromExpandedGrammar(grammar)  // fail on Left
  val lexers     = PerStateLexer.forTable(table, cache)       // Map[parseStateId, DFA]
  val extras     = Extras.build(...)
  '{ new ParserImpl[A](${ Expr(table) }, ${ Expr(lexers) }, ${ Expr(extras) }) }
}
```

### `ParserImpl` runtime (integrated lex + parse)

Unlike v1, there is no `lexer.tokenize(source)` upfront. The loop:

1. Start at `grammarState0` with unread `Source`.
2. Look up `lexers(currentParseState.id)` → run on source at current position.
3. Match returned token against `currentParseState.lookAhead` (may need `maxLookAhead` tokens buffered).
4. Shift / reduce / accept per table actions.
5. On shift to new parse state, switch to that state's lexer for the next token.

### Tasks

- [ ] Implement `ParserImpl` with integrated lex+parse loop
- [ ] Token buffer for `maxLookAhead > 1` disambiguation
- [ ] Remove debug `errorAndAbort` paths
- [ ] `ParserSpec` compiles and runs
- [ ] Parse at least one simple `Program` in tests

**Done when:** `Parser.derived[Program](2)` works end-to-end.

---

## Phase 7 — Tests and cleanup

- [ ] Fill in `ParserSpec.testSpec`
- [ ] Add layer-specific tests:
  - [ ] `FromExtractedTypeSpec`
  - [ ] `ParsingTableSpec`
  - [ ] `LexerSpec`
- [ ] Remove all `FIX-PRE-MERGE` markers and commented v1 paste-blocks
- [ ] Remove temporary debug dumps from `DeriveParser`

**Done when:** CI runs `slyce-core-v2/test` green.

---

## Suggested first PR

Smallest useful slice — proves the bridge works before touching LR tables:

1. Phase 0 (hygiene)
2. Phase 1 (grammar types)
3. Phase 2a only (`ProductNonTerminal`, `SumNonTerminal`, `ElementList`, `ElementOption`, terminals)
4. `DeriveParser` dumps expanded grammar for `ParserSpec.Program`

**Estimated effort:** ~3–5 days focused.

---

## Reference — v1 source locations

| v2 needs | v1 location |
|----------|-------------|
| Grammar expansion | `modules/slyce-generate/.../grammar/ExpandedGrammar.scala` |
| Parsing table | `modules/slyce-generate/.../grammar/ParsingTable.scala` |
| Lexer NFA/DFA | `modules/slyce-generate/.../lexer/NFA.scala`, `DFA.scala` |
| Reduce metadata | `modules/slyce-generate/.../output/Extras.scala` |
| Scala codegen (reference only) | `modules/slyce-generate/.../output/formatters/scala3/` |

## Reference — v2 key files

| File | Role |
|------|------|
| `generate/ExtractedType.scala` | Type classification (done) |
| `generate/ExtractedTypeCache.scala` | Memoized type walk (done) |
| `generate/DeriveParser.scala` | Macro entry point |
| `generate/grammar/FromExtractedType.scala` | **To write** — extraction → grammar |
| `test/.../ParserSpec.scala` | Integration test grammar |

---

## Progress log

| Date | Phase | Notes |
|------|-------|-------|
| | | |