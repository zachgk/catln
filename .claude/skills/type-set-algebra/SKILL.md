---
name: type-set-algebra
description: >
  Use when working on the core type-set algebra in src/Semantics/Types.hs — union, intersect,
  subset (isSubtypeOf), complement, difference, compactType, or the helpers they call
  (differencePartial, intersectPartialsBase, expandClassPartial, foldRecursiveType, predImplies,
  the satGoal engine) — or on test/Semantics/TypesTests.hs, or when a set-algebra LAW fails
  (commutativity, distributivity, De Morgan, A∪¬A=⊤, A∩¬A=∅, A−B=A∩¬B, A∩B=A−(A−B), …). Covers
  the set-theoretic model (types as sets, partial types vs types, type variables, predicates,
  arg modes, open vs closed world), the exactness contract, and the TypesTests dev loop.
---

# Catln types as set algebra

Catln types ARE sets of values, and `src/Semantics/Types.hs` is their Boolean algebra. This is
the layer *beneath* the constraint solver: `debug-typechecker` covers inference over points/epochs;
this skill covers the set operations that inference (and TreeBuild, and Eval dispatch) calls into.
`test/Semantics/TypesTests.hs` is the correctness contract for that algebra — property-based, over
types generated from real fixture programs.

## The exactness contract (the one rule that governs everything here)

**Every property in `TypesTests` must hold EXACTLY for every generated input.** The properties are
the laws of set algebra. Any input for which a law fails is a **bug in the operation** (or a helper
it calls) — it is NOT:

- a limitation to document,
- a generator to restrict so the case stops being produced,
- a "conservative" / "approximate" branch to paper over, or
- an assertion to weaken.

Fix the operation at the root so the law holds for that input class, then keep going. The ONLY
legitimate reason to exclude a generated value is that it is genuinely not a well-formed `Type` the
compiler can ever produce — and even then you exclude it by making the **generator produce
well-formed values that match how the compiler normalizes**, never by weakening the law. If you
think an input is malformed, *prove it*: find where the compiler rejects/normalizes it and mirror
that in the generator.

Coverage is secondary and follows automatically: richer generators + exact operations ⇒ the
difference/complement/predicate/mixed/var branches all get exercised. The driver is EXACTNESS.

## The set-theoretic model

### `Type` — the representation (`Types.hs` ~L237)

```haskell
data Type
  = UnionType (Maybe (TypePredicates, PartialLeafs, [Constant])) PartialLeafs [Constant]
  | TypeVar TypeVarAux TypeVarLoc Type   -- the third field is the occurrence-local `inner`
```

`UnionType mTop posLeafs posConsts` denotes:
- `Nothing`: `posLeafs ∪ posConsts` — a plain positive union of partials and constants.
- `Just (preds, negLeafs, negConsts)`: `(universe-filtered-by preds − negLeafs − negConsts) ∪ posLeafs ∪ posConsts`.

The **`Just` tuple is the entire subtractive/"top" component; the OUTER `posLeafs`/`posConsts` are
ALWAYS positive.** Do not confuse a top's exclusion consts (inside the tuple) with the outer positive
consts. (This split exists precisely so "class-top ∪ positive constant" is representable without
lossily expanding the class-top into its members.)

Key pattern synonyms — use them; don't hand-match the tuple:
- `PTopType` — the universe ⊤ (`Just (PredsNone, ∅, [])`, no positives). `¬PTopType = BottomType`.
- `BottomType` — ∅ (`Nothing` with empty leafs and consts). `containsBottomType` tests it.
- `TopType negLeafs preds` — a pure predicate/complement top (`Just (preds, negLeafs, [])`, no positives).
- `singletonType p` = `UnionType Nothing {p} []`; `partialVal n` = a bare `PartialType`;
  `relTypeVal n` = `TopType ∅ (PredsOne (PredRel …))` — "isa n".

### Partial types vs Types (`PartialType`, ~L179)

A `Type` is a *union* of alternatives. A **`PartialType`** is ONE alternative — "what a single object
or data declaration could match":

```haskell
data PartialType = PartialType
  { ptName    :: TypeName
  , ptVars    :: HashMap TypeVarName Type   -- type PARAMETERS  (e.g. the $T in Cons[$T])
  , ptArgs    :: HashMap ArgName Type       -- data FIELDS      (e.g. head/tail on Cons)
  , ptPreds   :: TypePredicates
  , ptArgMode :: PtArgMode
  }
```

**`ptVars` and `ptArgs` are matched INDEPENDENTLY.** A value's field (`ptArgs`) is *not* tied to a
type parameter (`ptVars`) through the data definition during set ops — they are separate slots. In
storage, partials sharing a name are grouped as `PartialLeafs = HashMap TypeName (HashSet
PartialArgsOption)`.

### Type variables (`TypeVarAux` / `TypeVarLoc`, ~L275)

- `TVVar name` refers to a `ptVars` parameter; `TVArg name` refers to an argument's type.
- `TVApplied base overrides` = a var re-applied with rebound parameter slots (`$F[$T -> $_B]`).
- **Location** decides scope: `TVInt` (written `$T`) resolves against the *same* partial's
  `ptVars`/`ptArgs`; `TVParent` (`$^T`) names a variable of the nearest ENCLOSING partial and never
  consults the vaenv; `TVExt` (`$_T`) resolves against the surrounding `TypeVarArgEnv` (vaenv).
- The third `TypeVar` field is the occurrence-local **inner**: the occurrence denotes
  `⟦var⟧ ∩ ⟦inner⟧`, and `inner = PTopType` is the knowledge-free case. It bounds an occurrence; it
  does **not** stand in for resolving the variable (see the resolution rule below).

**Resolution is an obligation of every traversal — and it has two failure directions.** `ptVars` is
a binder, and a reference to it sits somewhere below the partial that binds it, so any traversal
descending past that partial must carry the binding down (`intersectPartialsBase`'s `deepVaenv`,
`isSubPartialOfWithEnv`'s `readOwnScope`/`comparedPartialsEnv`). Getting it right means neither:

- **too lenient** — reading the reference as ⊤. `vaenvLookup` defaults a miss to ⊤, which is
  correct for a genuinely free variable and silently wrong for one whose binder you failed to carry.
  Since ⊤ is the lattice top the error only ever widens, so membership over-approximates and
  difference (`a ∩ ¬b`) *under*-approximates — a real narrowing reads as ∅; nor
- **too conservative** — freezing the occurrence at the variable's declared bound. `$/E` under
  `[/E=∀Ord]` is not "some Ord"; it is the same type as every other `/E` occurrence, and where the
  call site instantiates it that type is `Integer`. Replacing it with `∀Ord` throws away the
  instantiation and the cross-occurrence link, and the loss reappears downstream as `∀Ord` where
  `Integer` was needed.

A variable's **value is contextual** and must be threaded. Its **scope class** (`TVInt`/`TVParent`/
`TVExt`) is lexical, assigned once at desugar, and deliberately never re-derived inside the algebra
— do not confuse the two. No representation change excuses a traversal from resolving correctly.

**The `mergeVarEnvVal` doctrine (critical for the algebra props):** a *bare top-level* variable meet
`$a ∩ b` keeps `$a` symbolic and records the narrowing in the (often discarded) env, rather than
collapsing to the meet — deliberately wider, so a call-site lambda's parameter can be refined later.
Because the laws are about *denotations*, `TypesTests` reads a bare top-level variable to its value
first (see `readTopVar`/`intersectR` in the test module) and leaves NESTED variables for the ops,
which resolve them exactly by threading the vaenv through each scope. When adding a var case, respect
this: bare-top vs nested are handled in different places.

### Predicates (`TypePredicate` / `TypePredicates`, ~L131)

The `preds` inside a top filter the universe. Atoms:
- `PredClass p` — "hasClass C".
- `PredRel p` — "isa R" (`val : classOrModuleOrType`).
- `PredExpr p` — an expression predicate.

Combined with `PredsOne` / `PredsAnd [..]` / `PredsNot`. A **resolved type name is a positive leaf,
never a `PredClass`** — mirror `resolveRelPartial` (Desugarf) when generating. Implication and
satisfiability over these atoms are decided by the symbolic engine, not by enumeration (see below).

### Constants

`CInt`/`CFloat`/`CStr`/`CChar` are singleton value sets. They appear positively (outer list) or as
exclusions from a top (inside the tuple). `compactType` is responsible for cancelling a positive
constant against a covering top / a matching negative constant.

### Arg modes (`PtArgMode`, ~L117) — the interval lattice

Two independent booleans bound the arg-name set of a matching instance, as an interval in the subset
lattice:
- `ptArgLb` (lower-bounded): every listed arg is required (instance args ⊇ listed).
- `ptArgUb` (upper-bounded): no unlisted args allowed (instance args ⊆ listed).

`PtArgExact = (T,T)` (exactly the listed args); `PtArgAny = (T,F)` (listed required, extras allowed);
`(F,T)` any subset, no extras; `(F,F)` unconstrained. `..`/`??` forms come from `relaxNameBoundArgs`.
Difference and subtype across arg-mode intervals is genuine work — a name's realizable arg-sets are
**declaration-bounded** (see below), and vacuous interval bounds normalize away (`normalizePtArgMode`).

### Open world vs closed world (`tePrgmEnv`)

The default is **open-world**: types outside those currently defined may exist, so an **unsealed**
class is never enumerable and predicate implication over it must stay open-world sound. Only a
**declared `isa` edge** or a **sealed** class licenses enumerating members. `tePrgmEnv=True` switches
to closed-world (all classes effectively sealed) and is used only for declaration-coverage checks.
`teArgModeProjection` similarly loosens arg-mode matching for dispatch consumers (TreeBuild/Eval);
it stays `False` for the set-algebra so the laws remain sound. **Getting an open/closed-world gate
wrong is the single most common source of a law failing in one direction only.**

## Settled semantic principles (durable — lean on these when fixing an op)

- **Declaration-boundedness.** A name's realizable arg-sets and a field's value-set are bounded by
  its declaration, not read literally-unbounded. `typeGraphArgKeys` gives the declared arg-name sets;
  a leaf whose field exceeds its declared type is uninhabited (`partialUninhabitedByDecl`), and a sub
  field failing a literal subtype is clamped to its declared type before concluding. This is why
  `Point(x=⊤) ≡ Point(x=Integer)` and why over-wide leaves are ∅.
- **The sat engine is nominal and open-world.** `Cons(⊤,⊤) ⊆ ∀Bag` is decided by declared class/isa
  edges, not by reading fields. Implication over an unsealed class is open-world; a sealed class or a
  declared `isa` edge is what enumerates. Real compilation depends on the nominal reading — do not
  "fix" a law by making the engine read fields structurally.
- **Recursive tops fold to finite `PredRel` form.** An over-unrolled recursive union
  (`Nil + Cons(tail = Nil + Cons(tail=~ConsList))`) folds back via `foldRecursiveType`
  (gated by `hasExplicitRecursion`). `compactType` must reach the canonical depth-1 form so
  `propCompactIdempotent` holds.

## The operations and their entry points

| Op | Entry point(s) |
|---|---|
| union | `unionTypes` / `unionTypesWithEnv` |
| intersect | `intersectTypes` / `intersectTypesEnv` / `intersectTypesWithVarEnv` (returns `(vaenv, Type)` — records var bindings) |
| complement | `complementTypeEnv` |
| difference | `differenceTypeEnv` = `a ∩ ¬b` (via `intersectTypesEnv … (complementTypeEnv … b)`) |
| subset | `isSubtypeOf` / `isSubtypeOfWithEnv` |
| equality | `isEqType` / `isEqTypeWithEnv` (= mutual subset) |
| normalize | `compactType` (drops disjoint exclusions, cancels pos/neg consts, folds recursion, → ⊥ when negs cover the universe) |

`compactType` is the canonicalizer the compaction/idempotence props pin down: `compactType t` must
already be a fixed point, and re-compacting must not change it. Most "two equal types compare
unequal" bugs are really a missing `compactType` reduction on one side.

## Using TypesTests

### The laws (each is one `propXxx` in `TypesTests.hs`)

Reflexivity, commutativity, `compactType` no-change + idempotence, subtype-by-union/intersection,
distributivity both ways, `A−A=∅` / difference-shrinks / difference-subset, `A∪¬A=⊤` /
`A∩¬A=∅` / complement-inverse, and the two identity bridges `A∩B = A−(A−B)`
(`propIntersectByDifference`) and `A−B = A∩¬B` (`propDifferenceByIntersection`). Class/var-specific
props (`propParamClassCovariant`, `propSubtypeUnionVarSplit`, `propClassExpansionLeafSubtype`,
`propClassGraphMatchesPredClass`, …) pin the parameterized-class and expansion behavior.

### Generators (`src/Testing/Generation.hs`)

`genType prgm` picks from `gens` — `genBasic`, `genCG`/`genCGRel`, `genObj`/`genObjM` (from the
fixture programs' objects), `genConst`/`genMultiConst`, `genPredShape`, `genMixed`, `genTypeVar`,
and `genUnrolledRecursive` (over-unrolled recursive class). Type variables/vaenv draw names from a
shared fixed `varNamePool = ["a","b","c"]` so a bound fraction of `TVExt` slots actually resolve.
The empty program has no class nodes, so predicate/object/mixed gens run **only in the non-empty
branch** (they call `graphToNodes`/`genObj`, which crash on an empty graph — guard like
`genMultiConst` does). Fixtures live in `test/Semantics/code/` (`containers.ct`, `Animals.ct`).

To exercise an uncovered branch, add the generator/fixture that produces that input class — never
loosen a property to reach it.

### Dev loop — persistent ghci on a FIFO (a full `stack test` is ~160s startup; `:r` is seconds)

```sh
SCRATCH=<scratchpad dir>
mkfifo $SCRATCH/ghci-in
nohup sleep 100000 > $SCRATCH/ghci-in 2>/dev/null &        # holds the write end open
nohup stack repl catln:lib catln:test:catln-test < $SCRATCH/ghci-in > $SCRATCH/ghci-out 2>&1 &
# then append commands:  cat >> $SCRATCH/ghci-in <<'EOF' … EOF ; sleep N ; read $SCRATCH/ghci-out
```

Names are ambiguous across test modules — qualify (`Semantics.TypesTests.main`, `.typeTests`,
`.findPrgms`). Run ONE property at high count and capture the shrunk counterexample:

```haskell
import qualified Test.Tasty.Runners as TP
import qualified Test.Tasty.Patterns.Types as TT
defaultMain (localOption (HedgehogTestLimit (Just 5000)) $
             localOption (TP.TestPattern $ Just $ TT.ERE "propUnionCommutative")
             Semantics.TypesTests.typeTests)
```

Then reproduce the shrunk `Type`s directly in ghci — build them with `partialVal`, `singletonType`,
`TopType`, `relTypeVal`, `PTopType`, `BottomType` and call `unionTypes`/`intersectTypes`/
`differenceTypeEnv`/`complementTypeEnv`/`isSubtypeOf`/`isEqType`/`compactType`. Find the FIRST op that
returns a wrong/inexact result, trace into `Types.hs`, fix the root.

Harness gotchas:
- tasty ERE `|` alternation matches 0 tests — run one pattern at a time.
- The three `withDiscards 1000` props (`propSubtypeUnionVarSplit`, `propClassExpansionLeafSubtype`,
  `propClassGraphMatchesPredClass`) **discard-starve** if you raise `HedgehogTestLimit` without also
  raising `HedgehogDiscardLimit` — that "failure" is a harness artifact, not a real bug. For a clean
  high-count run: `localOption (HedgehogDiscardLimit (Just 200000)) $ localOption (HedgehogTestLimit (Just 3000)) …`.
- These are property tests — **not flaky**. Any failure is a real compiler bug. Keep the printed
  Hedgehog replay seed to reproduce it (`retest` in the module shows the `HedgehogReplay` form). To
  raise iteration count for a suite run, edit the `p` wrapper / `HedgehogTestLimit` and run ONCE at
  the larger count rather than re-running (avoids the ~160s startup each time).
- **Validate every fix at 5000 iters with TWO different seeds.** A fresh seed (e.g. a coverage run)
  routinely catches a seed-dependent regression that N same-session runs all miss — same-session runs
  share blind spots.

### HPC coverage (measure at ≥2000; property runs are seed-nondeterministic)

```sh
stack test --fast --coverage --no-run-tests                 # instrumented binary
BIN=$(ls .stack-work/dist/*/ghc-*/build/catln-test/catln-test)
HPCTIXFILE=$SCRATCH/tt.tix $BIN -p TypeTests --hedgehog-tests 2000
HPCDIR=$(ls -d .stack-work/dist/*/ghc-*/hpc)
hpc report --hpcdir=$HPCDIR --include=Semantics.Types $SCRATCH/tt.tix
hpc markup --hpcdir=$HPCDIR --srcdir=. --destdir=$SCRATCH/hpc --include=Semantics.Types $SCRATCH/tt.tix
```

(`stack test --coverage` writes back an empty Tix — run the binary directly, stack #1008.) When
reading coverage, three buckets are *legitimately* uncovered and should stay so: debug traces
(`teDebug`), defensive `error` guards, and dispatch/projection-only paths that the precise-algebra
props never enter (closed-world `gatePos`, `teArgModeProjection`, the materialized-expansion subtype
fallbacks the symbolic engine supersedes). A genuinely-reachable precise-algebra branch that's
uncovered means a missing generator input — add it.

## How to fix an operation (design principles)

- **No fallbacks.** A "conservative"/approximate branch that papers over a case is not a fix. Define
  the rule with simple, exact logic for the inputs it claims to handle.
- **Watch `_`-ignored inputs that are actually meaningful.** Either the rule is only correct for the
  narrower shape — then match that shape explicitly (e.g. `(H.null -> True) []` for "empty pos, empty
  consts") so it's obvious when the clause applies — or the dropped component is load-bearing for a
  later branch and must be consulted. In `isSubtypeOf`'s general Just–Just clause the `_ _` positions
  look ignored but the full `t1@`/`t2@` flow into `typeCoveredSymbolic` / the re-expression fallback;
  dropping them breaks real compilation. Verify a narrowed clause doesn't strand a later branch.
- **Delimit applicability with patterns, not one broad clause riddled with internal special-cases.**
- **Root cause over goal.** If a fix needs deep/cascading change or regresses unrelated tests, stop
  and raise it. Making the algebra stable and reliable outranks any single feature.

## Process discipline

1. One change → rerun the affected prop at high count → then the whole suite (all props) at ≥3000
   with a high discard limit, at two seeds.
2. `stack build --pedantic` must be clean for the **lib** (`--fast` does NOT enforce `-Werror`, so
   unused matches / incomplete patterns slip through and break pedantic later). Note: the test suite
   *outside* TypesTests may be WIP-broken — check TypesTests specifically, and confirm the Integration
   `.ct` failing set is byte-identical to baseline when a core op changes (open/closed-world flips
   and subtype changes affect real compilation).
3. Core `Types.hs` edits churn many goldens (every file's `unionAllObjs` shifts) — regenerate with
   `GOLDEN_TEST_WRITE=1 stack test` and confirm it's churn, not breakage, by checking assertions.
4. `make format`, then commit small (one law/branch per commit), and record the root cause in memory.
