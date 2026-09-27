---
name: idiomatic-catln
description: >
  Use when writing or restructuring Catln (.ct/.ctx) source — standard library or user code —
  and you want it to read idiomatically: literate nested doc comments, #example print-golden
  tests placed beside each component, generic class declarations implemented by member arms,
  the [ ] list sugar, if/case/match/guard branching instead of hand-rolled dispatch helpers,
  and the existing higher-order combinators instead of a fresh Nil/Cons recursion. Covers the
  syntax that actually works today and the compiler frontiers to route around — chiefly the
  TopType lambda-parameter rule that constrains all higher-order code.
---

# Writing idiomatic Catln

Catln is a term-rewriting language: you declare an operation once (often on an abstract class)
and define arms that pattern-match the concrete constructors. Idiomatic Catln leans on that —
generic declarations, small pattern-matching arms, and documentation and tests kept next to the
code they describe. This skill is the style guide plus the load-bearing syntax facts, each
verified against the compiler.

Everything here is checkable: put it in a `.ct`, run `stack exec catln -- test <file>` (runs
`#test`/`#example`/print-golden and property tests) or `stack exec catln -- run <file>` (type-checks
everything and evaluates `main` — faster, no property tests). See `debug-typechecker` when a type
error is the problem rather than the style.

## 1. Literate style: nest doc comments *inside* the statement

A comment indented **under** a declaration documents it (it desugars to the declaration's `#md`).
A comment **before** a declaration is just a floating section note. Prefer the nested form for
per-definition docs, and reserve leading `# ## Heading` comments for grouping sections.

```catln
sum(lst -> /Data/List[$T -> Integer]) -> Integer
  # [sum] adds up a list of [Integer]s. The empty list sums to zero; a non-empty list adds its
    [head] to the sum of its [tail].
sum(lst= Nil)= 0
sum(lst= /Data/Cons[$T -> Integer](head, tail))= head + sum(lst= tail)
```

- The doc attaches to the **signature declaration**; the pattern arms follow it unadorned.
- Continuation lines of one comment are indented one level under the `#` line (no extra `#`).
- Reference other definitions with `[name]` — it becomes a doc link (e.g. `[head]`, `[List]`).
- For an **inherited class** (a `Functor`, `Monoid`, …), explain *why this type qualifies*, not
  what the class is. "A [ConsList] is a [Functor]: [fmap] applies a function to every element while
  preserving the list's shape" — not a definition of functors.

## 2. Tests as `#example` print-golden, beside the component

Write examples as **print-golden** statements right after the definition they exercise. A line
`> expr` prints `expr`; an indented `>>> value` is the expected rendering, checked by
`catln test` and (re)written by `catln test --update`. They double as documentation (shown in
webdocs) and as tests.

```catln
> /Data/sum(lst= [1, 2, 3])
  >>> 6
> /Data/sum(lst= [])
  >>> 0
```

- Works nested inside a `module(...)` (grouped with the code) or at file top level.
- **`print` calls `toString` implicitly.** `ToString` is a subset of `DShow` (`every(/Data/ToString,
  isa= [/Catln/Doc/DShow])` in `webdoc.ct`), so a `> expr` whose value is any `ToString` type —
  `Integer`, `Boolean`, `String`, a `ConsList` of `ToString` elements — is rendered through its
  `toString` and golden-checked. **Do not write `.toString` in an example** (`> sum(xs)`, not
  `> sum(xs).toString`).
- Any `DShow` value can appear after `>` (that is the print syntax's type), but **only `ToString`
  values are compared against a `>>>` golden**. A `DShow`-but-not-`ToString` value (a webdocs
  visualization like `Show/MD`) is still displayed; it just produces no golden test (silently
  skipped).
- On the rare result that cannot render directly (see §8), compare instead:
  `> (expr == known)` prints the `Boolean` (`True`/`False`), which is `ToString`.
- **These are the tests for a stdlib operation.** A new `stack/core/` definition is covered by the
  print-goldens written beside it — don't add a parallel file under `test/Integration/code/` to
  re-test it. That directory is for exercising *compiler* behaviour (a syntax form, a dispatch
  path), not for re-asserting what a stdlib example already asserts.
- `#example` (annotation on a named value, holding `#assert`s) still exists and is right when you
  want property-style assertions rather than a shown value:
  ```catln
  exampleThing =
    #example
    #assert(test= <boolean>)
    0
  ```
  but the `>`/`>>>` form is preferred for "call → result" illustrations.

## 3. Declare on the generic class; implement with member arms

Declare an operation on the **abstract class** so every implementer shares the declaration, and
provide arms that match the concrete constructors. A future list type reuses the declaration by
being a `List`.

```catln
class(List[$T])
class(ConsList[$T], [Cons[$T -> $_T](head -> $T, tail -> ConsList[$T -> $_T]), Nil])
every(ConsList[$T -> $_T], isa= [/Data/List[$T -> $_T]])

reversed(lst -> /Data/List) -> /Data/List      # declared once on the interface
reversed(lst)= reverseOnto(lst= lst, acc= /Data/Nil)   # ConsList arms fulfil it
```

- **Arms fulfil the class declaration directly** — you usually do *not* need a second concrete
  `reversed(lst -> ConsList) -> ConsList` declaration. The `Nil`/`Cons` arms cover it.
- **Class membership** is `every(T[$T -> Bound], isa= [Class])`; make it *conditional* on the
  element when the arms need it: `every(ConsList[$T -> /Data/Algebra/Eq], isa= [/Data/Algebra/Eq])`
  makes a list an `Eq` only when its elements are.
- A **class method** is declared `:$T.method[$T -> Class](...) -> R` and defined per member
  (`:Integer.method(...)= …`). This is how `eq`/`equal`, `compareTo`/`ord`, and `toString` work.
- **Use absolute class names in a `[$T -> Class]` bound** (`[$T -> /Data/Algebra/Ord]`, not `Ord`).
  A relative name there fails with "Could not resolve required name".
- **Helper functions must be declared on a concrete type** (`ConsList`), not the open class — an
  open class demands total coverage across *all* members, so `reverseOnto(lst -> /Data/List)` with
  only `Nil`/`Cons` arms fails "Unfulfilled Declaration". Declare helpers on `ConsList`.
- To keep an open interface (like `Functor`) from requiring total coverage, put a type's instance
  in its **own submodule** using absolute names, e.g. `module(Data/List)` with `/Data/fmap(...)=`.

## 4. Branch with real syntax — don't hand-roll a dispatch helper

Catln has four branching forms. Reach for one before writing a `Boolean`-dispatched step
function; they all desugar into exactly that generated declaration
(`Syntax/Ct/Desugarf/Preprocessors.hs`, `if` via `/Catln/ThenElse/fromBool`), so a
hand-written one is duplicated boilerplate.

```catln
abs(x -> Integer)= if(x >= 0)     # if/Then/Else — test/Integration/code/ifThenElse.ct
  Then= x

  Else= -1 * x

abs(x -> Integer)= match(x)       # match — test/Integration/code/match.ct
  x2 | x >= 0= x2
  x2 else = x2 * -1

abs(x -> Integer)= case(x)        # case — test/Integration/code/case.ct
  x2 | x >= 0= x2
  x2= x2 * -1

abs(x -> Integer) | x >= 0= x     # arrow guards — test/Integration/code/cond.ct, fib.ct
abs(x -> Integer) else = x
```

- **Arrow guards are the default choice.** They are the form that works inside generic,
  recursive definitions, and they keep the decision on the function it belongs to. Examples in
  `stack/core/`: `Data/Map/insert`, `Data/Map/remove`, `Data/List/range`, `Data/List/sorted`,
  `Data/Set/intersectInto`.
- **A branch is a declaration, not an expression** (`stack/rss/main.ct`). It is a whole arm
  body — you cannot put an `if` inside an argument list or join one into a string
  mid-expression. `mmap(m= lst, fn(v)= if(...) ...)` is a *parse error*.
- **End a guard chain with `else`.** Spelling the last arm as another explicit guard gives
  "Unfulfilled Declaration" — a chain of opaque guards can't be proven total.
- **Prefer dispatching on constructors over testing a value** where the shape allows it. Arms
  that pattern-match `Nil`/`Cons`, `Leaf`/`Node`, `True`/`False`, or `Equal`/`LessThan`/
  `GreaterThan` need no guard at all, and they are checked for coverage.
- **Frontier — a `match`/`case` arm written as a constructor does not select.** The `match` and
  `case` preprocessors put each arm's pattern in as the alias of a hole (`_@pattern`), which
  constrains nothing, so constructor arms (`/Data/Algebra/Equal= …`) share one ⊤ domain: the
  last arm answers for every value, and in a generic definition the arms' derived overlap test
  fails with "eval failed". (`if` was fixed by moving the pattern to the argument position; doing
  the same for `match`/`case` currently breaks binder arms like `x2 | x >= 0= …`, because a bare
  binder in an argument-value position is not bounded by anything.) Also note `x -> Type= …` in
  a match arm is a binder with a declared *result* type, not a type pattern. Use arrow guards —
  that is why `Data/Map/get`/`insert`/`remove` are guarded.
- **Test a branch by running the branch that is not taken.** All four branching forms were
  silently broken — a guarded arm was taken whether or not its guard held — while `case.ct`,
  `match.ct`, `ifThenElse.ct` and `cond.ct` all passed, because each only ever evaluated a point
  where both arms return the same value. An example that cannot distinguish the arms is not a
  test of the branch.
- **Guards work in generic, recursive definitions.** An arm whose body is a recursive call once
  failed at runtime ("Found no matching conditions in evalTree") because a generic declaration's
  result was not seeded onto its definitions, so the recursive call was left unevaluated; that is
  fixed (`seedDefResultTypes`), and `Data/Map/get` is now guarded.

## 5. Prefer a combinator to a fresh recursion

A fresh two-arm `Nil`/`Cons` recursion is for **primitives only**: a `Functor`/`Monad`
instance, a BST descent (`get`/`insert`), an unfold (`range`), or two-list lockstep
(`equal`/`compareTo`). Anything else that walks a list should call a combinator.

The combinators live in `stack/core/`; grep `stack/core/list.ct` for the current set rather than
assuming one exists. At the time of writing:

| Combinator | What it does |
|---|---|
| `/Data/Functor/fmap` | map a function over any `Functor` |
| `/Data/Monad/mmap`, `pure` | concatMap / bind |
| `/Data/List/allBy`, `anyBy` | quantify a *predicate* over a list of any element type |
| `/Data/List/elem` | membership, via `anyBy` + `Eq` |
| `/Data/List/all`, `any`, `sum` | aggregate a list that already holds `Boolean`/`Integer` |
| `/Data/Map/mapValues` | map over a `BinTree`'s values |

A combinator call also states the intent. `allBy(lst= elements(s= a), fn(v)= contains(s= b,
item= v))` says "every item of `a` is in `b`" in one line; the equivalent `Nil`/`Cons` walk makes
the reader reconstruct that from the recursion.

- **Forward a function argument as `fn(v)= fn(v= v)`, never bare `fn`.** A bare `fn` fails with
  "Actual type contains bottomType". Reference: `bind.ct`, `test/Integration/code/typeApply.ct`.
- **Function arguments may take more than one parameter.** `fn(acc, v)` and `fn(acc, key,
  value)` parse, typecheck and evaluate; arity is not a limit.
- **The combinator set is deliberately small, and the gaps are compiler-blocked, not
  oversights.** Before adding one, check the TODO comments in `stack/core/list.ct` — the
  missing entries (`filter`, `concat`, folds) each record the spellings already tried and the
  error each produced. §6 explains why most of them fail.

## 6. The `TopType` lambda-parameter rule

The single biggest trap in writing higher-order Catln. Read this before writing any function
that takes a function.

A call-site lambda's parameter is typed from the callee's declared input for that position, and
**only when that type is fully concrete** (`TypeCheck/TypeUtils.hs`, `fnArgInputBound`, `guard
(typeFullyConcrete inTy)`). `typeFullyConcrete` (`Semantics/Types.hs`) rejects a type variable
*and* a class-predicate top — so `Boolean`, `/Data/List`, `/Data/Set`, `/Data/Maybe` all fail
it. Only bare leaves like `Integer`, `String`, `True`, `Nil` pass.

When it rejects, the parameter stays `TopType`. That survives **only** if the body passes it
straight into another unconstrained slot; feeding it to a declared slot fails with
"buildCallTree from top type" or "Failed to build arrow tree from /fn(/v=TopType) to …".

```catln
applyInc[$F -> /Data/Functor[$T -> $_A], $A, $B](lst -> $F, fn(v -> $_A) -> $_B) -> $F[$T -> $_B]
applyInc(lst, fn(v))= /Data/Functor/fmap(lst= lst, fn(v)= fn(v= v))   # works: pure pass-through
```

- **Write generic higher-order code as a pass-through** — hand the element to another function
  and hand that result on — and it works (`fmap`, `mmap`, `bindap`, `allBy`, `anyBy`). Make it
  *consume* the lambda's answer to pick a branch or fill a declared slot and it breaks.
- **Don't type a plain argument by a bare type variable in the same definition as a
  function-typed argument.** `f[$B](fn(v -> Integer) -> $_B, init -> $_B) -> $_B` fails with
  "Definition Outside Declaration … Undeclared part: `/f(/init)`" — the definition's input type
  comes out as a union that drops the function argument. Either ingredient alone is fine (two
  plain arguments may share a type variable; a function argument's *output* may be one, which is
  what `fmap`/`mmap` do). This is what blocks a fold-shaped signature.
- **Fix the element type to isolate.** If a generic combinator fails, write the same thing with
  the element pinned to `Integer`. If that version works, you have hit this rule; if it fails
  too, you have a different bug.
- Symptoms: "buildCallTree from top type", "Actual type contains bottomType", "Failed to build
  arrow tree from /fn(/v=TopType)", "Found no matching conditions in evalTree".

## 7. Nest single-use helpers inside the function that uses them

A helper used by exactly one function does not belong in the top-level namespace. Write the arm
with a **bare `=` and nothing after it**, then indent the helper's declaration and arms, and put the
**body as the final indented line**. Desugar (`nestedDeclPreprocessor` → `flattenNestedDeclarations`)
lifts that last expression up as the arm's body, and the preceding decls become sub-functions —
renamed with the parent's path (so they can't collide) and closed over the parent's arguments.

```catln
reversed(lst -> /Data/List) -> /Data/List
  # doc for reversed stays on the signature declaration
reversed(lst) =                                              # bare '=', body comes last
  reverseOnto(lst -> ConsList, acc -> ConsList) -> ConsList   # private helper decl + arms
  reverseOnto(lst= Nil, acc)= acc
  reverseOnto(lst= /Data/Cons(head, tail), acc)= reverseOnto(lst= tail, acc= /Data/Cons(head= head, tail= acc))
  reverseOnto(lst= lst, acc= /Data/Nil)                      # final indented line = the body
```

- **Use the `f(args) =` + final-line form, and nest under an *arm* (not the bare `f(args) -> R`
  signature).** The last indented statement must be a plain expression — it becomes the body; the
  earlier statements are the helper decls. (The one-line variant `f(args)= body` with helpers
  indented under also works and desugars identically, but the bare-`=` form is the canonical one and
  reads better with several helpers.)
- **The enclosing arm's argument names are curried into the helper** (so the helper closes over the
  parent's context). A parent arg the helper *itself declares* is skipped automatically (lexical
  shadowing), but a parent arg that collides with a name the helper *destructures inside its arms*
  still clashes. If the arm destructures `Cons(head, tail)` and the helper also binds `head`/`tail`
  in its own arm patterns, rename the arm's fields with `@` first (`f(lst= Cons(head@h, tail@t))= …`)
  so the curried names don't clash. Non-colliding curried args are harmless — the helper ignores them.
- **Mutually-recursive helpers** (A calls B, B calls A) nest as **siblings** under the one arm, so
  both names are in scope for the cross-reference.
- **Method arms work** (`:Cons.toString= …`). A helper under a method arm nests fine — the parent's
  element context is curried into the helper, so the helper's own element-`.toString`/`.compareTo`
  calls resolve.
- **Helpers generic in their element nest fine too**, including ones dispatched through `Ord`/`Eq`
  (`insert[$T -> /Data/Algebra/Ord]` sits inside `sorted` in `stack/core/list.ct`). If nesting one
  produces a `Scheme failed verification … Grows by` at some unrelated definition, that is a bug
  worth a fresh minimal repro, not an expected limit.

## 8. Syntax quick reference & gotchas

- **List sugar:** `[1, 2, 3]` desugars to `Cons(1, Cons(2, Cons(3, Nil)))`; `[]` is `Nil`. Use it.
- **Named args are the norm:** `f(x= 5)`, `Cons(head= 1, tail= Nil)`. Positional field patterns
  bind by field name: `Cons(head, tail)`.
- **Alias a destructured field with `@`** (no space): `Cons(head@h, tail@t)` binds `head`→`h`.
  `field= name` means "destructure the field against pattern `name`", which is different.
- **Higher-order args include the nested parameter:** write `fn(v)` (with its parameter), not bare
  `fn`, in a definition that takes a function-typed argument.
- **Mixed-singleton lists unify.** `[True, False]`, `[Some(value= 1), Nothing]` and a `Dir` holding
  both a `File` and a `Dir` all type-check, given a declared element type to widen to.
- **A field is read by destructuring, not by an accessor.** `Dir(name, entries)` in an arm pattern
  reaches `entries`; `d.entries` reaches nothing, whether or not the list under it is mixed. `.name`
  works only where something is *declared* as a method (`:/Data/List.length`). This is what makes
  `/Data/FileOrDir` look unusable at first — the accessor fails, not the type.
- **Rendering a generic-element result usually works — check before routing around it.** Lists with
  a generic element render fine: `> /Data/List/sorted(lst= [3, 1, 2])` prints `[1, 2, 3]` and
  `reversed` likewise, both live goldens in `stack/core/list.ct`. Where a result genuinely cannot
  render (a `Set` built through the generic `add`, whose item type stays the abstract `Ord` bound —
  see the note in `stack/core/set.ct`), compare instead: `> (expr == known)` prints `True`, which is
  `ToString`. Try printing first; only fall back to comparison when it actually fails.
- **Formatting:** `stack exec catln -- fmt <file>` formats one file; `make ctformat` formats the
  whole repo (large diff — mention unrelated churn rather than committing it). `.ct` uses 2-space
  indent.

## 9. Process

1. Write the definition + its nested doc + a print-golden example together.
2. `stack exec catln -- run <scratch importing core>` to type-check fast, then
   `stack exec catln -- test <file>` to check the examples and property tests.
3. **Verify examples are actually checked, not skipped** — a print-golden on a non-String value is
   silently skipped. Corrupt a golden once and confirm it fails, or watch the passed-count rise.
4. Editing `stack/core/` churns goldens for every importing file; regenerate with
   `GOLDEN_TEST_WRITE=1 stack test` and confirm real pass/fail, not just the diff.
5. Finish Haskell-side changes with `stack build --pedantic` (`--fast` skips `-Werror`).
