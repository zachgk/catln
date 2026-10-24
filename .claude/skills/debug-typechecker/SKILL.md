---
name: debug-typechecker
description: >
  Use when debugging the Catln Haskell type checker — a .ct program type-checks to the
  wrong type, resolves to TopType, fails with "ACT ∅" / "Actual type contains bottomType" /
  "buildCallTree from top type" / "Failed to get singleton for TopType" / a scheme
  verification error, or the constraint solver does not converge (the epoch limit / "Grows
  by" monotonicity). Covers writing minimal repros that do not rebuild the standard library,
  telling which pipeline stage a failure or hang is actually in, the failure-signature
  catalog, how to trace points and constraints, and the constraint-solving code map.
---

# Debugging the Catln type checker

The type checker turns a desugared program into constraints over integer **points**, then
solves them in epochs. Most "wrong type" bugs are a point that ends up `TopType` or `∅`
(bottom) when it should be concrete. This skill is the loop for finding *which* point and
*which* constraint did it.

Pipeline: Parser → Desugar → **TypeCheck** → TreeBuild → Eval. A type that is wrong after
TypeCheck typically surfaces as a confusing error in TreeBuild or Eval.

## 0. The two invariants — check every conclusion against them

1. **Every scheme is an upper bound** on the possible values of its point. A stored ACT
   that excludes a value the term can actually take is a bug at the *write that excluded
   it*, not at the later constraint acting on the too-narrow type.
2. **Inference only narrows.** Each epoch's write must be a subset of the previous value
   (`verifyScheme` enforces this). So the FIRST too-narrow write is the root cause;
   everything downstream is sound reasoning from a wrong premise, and no later epoch can
   undo it — an omitted alternative dies in every later intersection.

**How to read an ArrowTo destination union (raw call forms).** A union like
`Nil ∪ fmap(⊤,⊤)` enumerates the *forms* the term may take: it is either the
still-unevaluated call form `fmap(⊤,⊤)` or one of the evaluated forms listed beside it.
The raw member does **not** mean "or whatever fmap produces" — the evaluated members are
the complete enumeration of what evaluation can yield, so this type asserts the call
evaluates only to `Nil`. This is the nuance in interpreting ArrowTo results in conjunction
with the two invariants: a forward write must include every applicable arrow's result
(`Nil ∪ Cons(…) ∪ fmap(⊤,⊤)`) or not narrow at all.

Practical consequence for debugging: on a missing dispatch arm or "Failed match in eval
callTree", walk the point's trace **backward** to the first write whose union omitted an
alternative — that write is the bug. (Known instance: the type-graph query's
most-specific-promise pruning can drop a definition arm whose object pattern is still
unresolved (it presents ⊤ and looks like a generic declaration), so the reach union is
written incomplete — see `mostSpecificPromises` in `src/Semantics.hs`.)

## 1. Reproduce minimally

**Get out of the standard library first.** Every `catln` invocation on a page that imports
core rebuilds the whole library, which is 2.5–5 minutes per run; a repro that avoids it runs
in well under a second. A page is given `import "core"` unless it writes `annot(#noCore)`
and then `#noCore` (`Syntax.Parsers.pageImports`), so a repro naming nothing from the library
still pays for it until it says so. Two things about where a repro lives: `core` resolves
relative to the file and then to `<stackPath>/core`, so a file OUTSIDE the worktree cannot
find the library at all (keep repros in `build/scratch/<name>/`, which is gitignored, and run
from the worktree root); and the whole DIRECTORY is compiled, so a neighbouring file's
`import "core"` is also yours — one repro per directory.

A `#noCore` file can *run*, not just type-check. Declaring the two types eval looks for is
enough to exercise inference, tree build and evaluation:

```catln
annot(#noCore)

#noCore

module(Data/Primitive)
  data(Integer)

module(Catln)
  data(IO)

g(x -> Integer) -> IO= IO

main -> IO= g(x= <the expression under test>)
```

`main -> IO` reaching `/Catln/IO` is what `evalPrgmTargetMode` needs to pick `EvalRun`; the
usual `main{io -> IO}` form needs core's whole `Context` machinery. The run ends in "Eval did
not return an instance of IO … TupleVal /Catln/IO" — that is this harness's own tail, not a
failure, and everything before it really ran. Where a repro has to report a value and core is
in play anyway, `_<- exit(val= …)` makes the process exit code the answer, which needs no `==`,
`#assert` or `String`.

**Which stage is at fault**, before theorising about any of them: `catln epochs FILES` stops
after type inference (and reports epochs per component), `catln tbuild` adds the arrow-tree
build, and `catln run` adds evaluation. Run the repro and a working control through all
three — the first command whose time or outcome diverges from the control names the stage,
and it is not always this one.

- Write the smallest standalone `.ct` that fails, importing core, with a `#test`/`#assert`:
  ```catln
  import "core"

  testX =
    #test
    #assert(test= <smallest failing expression>)
    0
  ```
  Run it: `stack exec catln -- test /tmp/x.ct` (use `run` instead if it has a `main`).
  `test` also runs Hedgehog property tests over the file's functions, so `✓ passed 100
  tests` lines are normal noise.
- **Replace user types with throwaways** to isolate: `data(Box[$T](v -> $T))` for a generic
  container, `data(Foo)` for a concrete one. Compare a parameterized type vs a concrete one —
  if concrete works and parameterized doesn't, suspect type-variable binding (see §6).
- **In-module vs imported matters.** A `stack/core/` function can type-check and evaluate
  correctly when its test lives in the *same* module (add the `#test` to the core file and run
  `stack exec catln -- test stack/core/data.ct`) yet fail when imported into another file.
  Always check both — the difference is itself a clue (cross-module re-resolution loses info).
- **If *every* file fails identically — even a trivial one like `arith.ct` — suspect the
  environment, not your program.** The CLI loads the standard library from the working
  directory's `stack/` during development (only an installed compiler falls back to
  `~/.catln/stack`), so a type error's file path tells you *which* core is in play. Sanity-check
  by running an unrelated passing file before trusting a failure on your repro.
- **`test` vs `typetrace` on failure.** Type errors — including `Scheme failed check`,
  `Scheme failed verification` (Grows-by), and `Definition Outside Declaration` — let
  `typetrace` write its files; use it (a scheme failure poisons its point but no longer aborts
  the service). A *hard* exception (`catln: user error (…)`, `Could not build catln service`)
  still occurs for parse/desugar errors and decode argument-inference failures, where there is
  no trace to write; run `stack exec catln -- test <file>` to see the underlying `CRes` error.

## 2. Read the failure signature

| Signature | Usual meaning |
|---|---|
| `... :: ACT TopType` (where you expected a concrete type) | Inference never narrowed the point — a variable or argument failed to propagate/bind. |
| `Scheme failed check at setScheme ... ACT ∅` / `Actual type contains bottomType` | The point collapsed to empty: an intersection/bound had nothing in common. Often a *consequence* of a var staying `TopType` (so `concrete ∩ <something built from Top> = ∅`), or a genuine type error. |
| `buildCallTree from top type` (`TreeBuild.hs`) | Type-checked to `TopType`, then TreeBuild can't pick an arrow for `Top`. Root cause is upstream in TypeCheck — fix the `TopType`, not TreeBuild. |
| `Failed to get singleton for TopType` (`Types/Construct.hs`) | Code needed one concrete partial but got `Top`. Same root as above. |
| `Reached runConstraints limit` (≈15 epochs) | Non-convergence/oscillation. Classic case: a recursive type unrolls one level deeper each epoch (`Nil + Cons(tail = Nil + Cons(tail = …))`). Non-fatal warning but real. |
| `verifySchemeActLowers` / "Grows by `<diff>`" | Monotonicity violation: a point's ACT *widened* between epochs (it must only narrow). Often two passes fighting (one narrows, one re-widens). |
| `Multiple matches` / `Failed match in eval callTree` (`Eval.hs`) | Runtime dispatch (`TCMatch`/`TCCond`) found 0 or >1 arrows for a value — a value-vs-arrow keying mismatch, not necessarily a type error. |
| `unionAllObjs (the TopTop) remained TopType` | Some object in the file failed to resolve to a concrete type, so the union-of-all-objects (used as the `BoundedByObjs` bound) collapsed to `Top`. A Top `TopTop` silently disables `BoundedByObjs` for the whole file, so this is checked and reported at convergence. Find the unresolved object (commonly an untyped binding leaking into the universe). |

The error block usually prints the offending point plus
`Constraining: <constraint> {… ACT <before> …}  to:  {… ACT <after> …}` — that line names the
exact constraint that emptied or Top'd the point. Start there.

## 3. Trace points and constraints

The solver is over integer points; each `Constraint` updates a point's `Scheme`, printed as
`ACT <inferred/upper type>; REQ <required bound>`.

- **`catln typetrace` (start here).** Run `stack exec catln -- typetrace /tmp/x.ct`. It writes
  the full inference trace (the text equivalent of the webdocs `TypeInfer` view) to
  `build/typetrace/`:
  - `program.txt` — every definition with the point id, type, and source position of each of
    its sub-expressions. **Use this to map the wrong expression to its point number N.** (Grep
    for the source line/column of the bad expression, or for the value name.)
  - `points-index.txt` — `pN @ <pos> :: <final scheme>` for every point; skim for the `TopType`
    / `∅` ones.
  - `pnt/<N>.txt` — for point N: its `Initial:` scheme, then per `Epoch i:` the exact constraint
    that updated it and the resulting `{… ACT …; REQ …}`, then every constraint touching it, then
    its `Neighbors:` (the other points sharing a constraint) with their final schemes. This is
    the whole "which constraint emptied/Top'd this point, and what were its inputs" loop in one
    file — read it instead of adding trace prints. Constraints are shown by point (`p20 ⊆_Known
    ~d`, `(p2)(TVArg /io) == p0`) with the constraint's operator id, so you can follow the same
    point across files and back to `program.txt`.

  Reach for the manual `Debug.Trace` options below only when you need value-level detail that the
  trace doesn't capture (e.g. *why* an intersection produced a given type — the `[SUBTYPE]` /
  `[INTERSECT]` steps inside a single constraint).
- **Broad value trace:** enable `teDebug` to get `[SUBTYPE]` and `[INTERSECT]` traces. It is
  set via `defaultTypeEnvDebug` in `mkTypeEnv` (`src/Semantics.hs`); flip it to `True`
  (or construct the `TypeEnv` with `teDebug=True`). Very verbose — scope to a tiny repro.
- **Targeted trace (preferred):** add a `Debug.Trace.trace` at the specific spot and **filter
  by name** to cut noise, e.g. inside `computeConstraint` (`src/TypeCheck/Constrain.hs`), or at
  `arrowDestType` / `exprVarArgsWithSrc` / `intersectTypesWithVarEnv`, printing the inputs and
  result:
  ```haskell
  | "fmap" `Data.List.isInfixOf` show x && Debug.Trace.trace (printf "...%s" (show v)) False = undefined
  ```
  (Add `import qualified Debug.Trace` / `import qualified Data.List`; remove before finishing.)
- **Instruments (`src/TypeCheck/Instruments.hs`)** are how a solve is watched. `TraceInstrument`
  records per-point scheme changes per epoch (the `TraceConstrain` `typetrace` writes) — useful for
  the oscillation/monotonicity cases; `EpochCount` counts epochs; `LastEpochs` keeps only what a
  non-convergence failure reports and is the one every typecheck carries. To add a measurement of
  your own, write an `Instrument` instance (`src/TypeCheck/Common.hs`) and attach it — the solver
  behaves the same whichever are attached.

Goal: find the point whose ACT becomes `∅`/`TopType`, then read which constraint did it and
why (what was intersected with what).

## 4. Code map (where to look)

- **`src/TypeCheck/Encode.hs`** — `fromExpr` / `fromObjArr` / `fromObjectMap` build constraints.
  `fromExpr (TupleApply …)` (≈line 122) is the hot spot: a multi-arg call `f(a)(b)` is a nested
  `TupleApply` where the **final** arg is the applied `EAppArg` and **earlier** args live in the
  *base*. The base is linked with `EqPoints` in `EncodeIn` (patterns/declarations) but `ArrowTo`
  in `EncodeOut` (expressions/call sites) — an asymmetry behind several non-final-arg bugs.
  `AddArg`/`PropEq` attach an argument's value and type.
- **`src/TypeCheck/Constrain.hs`** — `runConstraints` epoch loop (`Constrain.hs:239`), driven by
  `runConstraintsLimit` in `src/TypeCheck.hs`. `computeConstraint` handles each constraint kind:
  `EqPoints`, `AddArg`, `PropEq`, `BoundedByObjs`, `ArrowTo`, `BoundedByKnown`. `setScheme` /
  `verifyScheme` apply and check (monotonicity).
- **`src/TypeCheck/TypeUtils.hs`** — `arrowConstrainUbs`: how `ArrowTo` recomputes a partial
  application through the type graph (via `reachesPartials`). This *recompute* is why a
  non-final argument's call-site content can be dropped.
- **`src/Semantics/TypeGraph.hs`** — `reaches` / `reachesPartials`: what a partial resolves to
  through the type graph.
- **`src/Semantics.hs`** — `typeGraphQueryWithReason` (intersect a source partial with each
  arrow's object, then `arrowDestType`); `arrowDestType` (substitute the var env into the
  arrow's result).
- **`src/Semantics/Prgm.hs`** — `exprVarArgsWithSrc`: builds the variable bindings by matching an
  arrow's object pattern against a source partial. This is **where type variables bind — or
  fail to.** Note: it special-cases a *bare* `TypeVar` argument type but matches structured arg
  types only structurally.
- **`src/Semantics/Types/`** (the type algebra; module map in the `type-set-algebra` skill) —
  `intersectTypesWithVarEnv` (structural intersection returning
  `(TypeVarArgEnv, Type)` — records var bindings), `isSubtypeOfWithEnv`, `compactType`,
  `expandPredicates`. **Type shape:** `UnionType (Maybe (preds, negLeafs)) posLeafs consts`; a
  partial carries `ptArgs` (data fields) and `ptVars` (type parameters) **separately**, matched
  independently — a value's field is *not* related to a type parameter via the data definition.
- **`src/TreeBuild.hs`** — arrow selection / `buildCallTree` (where `TopType` blows up).
- **`src/Eval.hs`** — `evalCallTree` (`TCMatch` / `TCCond` runtime dispatch via
  `isSubtypePartialOf`); keying mismatches between runtime values and typed arrows surface here.

## 5. Process discipline (this matters — the type checker is regression-prone)

1. **Confirm the mechanism with a trace before editing.** A plausible "symmetric" fix often
   breaks the working path (e.g. making `EncodeOut` mirror `EncodeIn` regressed the final
   argument). Know exactly which point/constraint is wrong first.
2. Make **one** change → re-run the minimal repro → run the full suite (`stack test --fast`).
3. Core edits churn many goldens (every file's `unionAllObjs` shifts). Regenerate with
   `GOLDEN_TEST_WRITE=1 stack test`; verify it's churn (not breakage) by confirming individual
   tests' assertions still pass.
4. Finish with `stack build --pedantic` — `--fast` alone does **not** enforce `-Werror`, so
   warnings (unused matches, incomplete patterns) slip through and break the pedantic build later.
5. If the fix needs deep/cascading changes or regresses unrelated tests, **stop and raise it**
   rather than patching.

## 6. Parameterized types & destructure bindings

Two recurring, generalizable traps when a destructured or parameterized value's *element*
type goes wrong (an element staying `TopType`/`/name`, a destructuring definition that never
reduces, or a spurious coverage error):

- **val vs arg (matching-criterion vs matched-value).** In `f(field= value)` /
  `Type(field@name)`, the `field` (and the function/constructor head) is a *val* — a matching
  criterion / object — while the matched `value`/binding is an *arg*. The
  `exprVarArgs`/`exprVarArgsWithSrc` builders (`Prgm.hs`) and `desObjValToArg` (`Desugarf.hs`)
  classify these. A binding keyed under the wrong name — the field instead of its alias, or
  vice-versa — is a common cause of an unresolved binding or a value that never reduces. An
  aliased field (`x@v`) must be keyed by the alias (`v`) in *every* such builder (the constraint
  var-env, the with-src var-env, and the eval `texprArgsWithVal`), or a *use* of the alias won't
  resolve even when a *return* of it does.
- **A type variable's slot should hold its declared bound, not a binding.** If a parameter shows
  up like `A=(PredRel <name> && <Bound>)` — a `PredRel <bindingName>` (a value binding)
  intersected with a real class/type bound — a binding has leaked into a bound slot. A slot is
  one or the other: the matched binding (key side) or the declared bound (value side), never
  both. That signals mis-routed field↔param plumbing (`updateTypeProp` /
  `intersectTypesWithVarEnv`, `Types/Access.hs`, `Types/Algebra/SetOps.hs`). Relatedly, a declared field is completed with its type
  parameter's *bound* (`completeDataPartials`); a value/definition is compared
  against that in the decl/def coverage check (`verifyObjMapItem`, `Decode.hs`).
