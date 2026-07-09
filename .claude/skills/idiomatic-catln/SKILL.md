---
name: idiomatic-catln
description: >
  Use when writing or restructuring Catln (.ct/.ctx) source — standard library or user code —
  and you want it to read idiomatically: literate nested doc comments, #example print-golden
  tests placed beside each component, generic class declarations implemented by member arms,
  the [ ] list sugar, and if/case/match/guard branching. Covers the syntax that actually works
  today and the compiler frontiers to route around (recursive if/else, rendering
  polymorphic-element results).
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
- When the result type is too polymorphic to render directly (see §7), compare instead:
  `> (expr == known)` prints the `Boolean` (`True`/`False`), which is `ToString`.
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

## 4. Nest single-use helpers inside the function that uses them

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
- **Mutually-recursive helpers** (A calls B, B calls A — e.g. a `rangeFrom`/`rangeStep` pair) nest
  as **siblings** under the one arm, so both names are in scope for the cross-reference.
- **Method arms work** (`:Cons.toString= …`). A helper under a method arm (e.g. `restToString` for
  `toString`) nests fine — the parent's element context is curried into the helper so its own
  element-`.toString`/`.compareTo` calls resolve. (This used to fail with `Actual type contains
  bottomType`; fixed by the call-site currying in `currySubFunctions`.)
- **Frontier — keep generic `Ord`/`Eq`-dispatched helpers at top level.** Nesting a helper that is
  generic in its element (`insert[$T -> /Data/Algebra/Ord]`) or wired into the `Eq`/`Ord`/`Comparison`
  machinery (`consThen`, used inside the generic `Cons.compareTo` arm) tips the constraint solver
  into a global `Scheme failed verification … Grows by` that surfaces at an *unrelated* definition.
  This is a deeper solver-instability frontier, **independent of nesting** — a monomorphic
  (`Integer`) version of the same helper nests cleanly. When nesting produces a "Grows by" error,
  revert to top level. Both nesting forms behave identically here (the failure is in the solver, not
  the body syntax).
- **Net:** non-generic helpers nest cleanly under plain *and* method arms (`reverseOnto`/`reversed`,
  `rangeFrom`+`rangeStep`/`range`, `restToString`/`toString`); only helpers tied to generic
  `Ord`/`Eq` dispatch (`insert`, `consThen`) stay at top level until the solver frontier is resolved.

## 5. Syntax quick reference & gotchas

- **List sugar:** `[1, 2, 3]` desugars to `Cons(1, Cons(2, Cons(3, Nil)))`; `[]` is `Nil`. Use it.
- **Named args are the norm:** `f(x= 5)`, `Cons(head= 1, tail= Nil)`. Positional field patterns
  bind by field name: `Cons(head, tail)`.
- **Alias a destructured field with `@`** (no space): `Cons(head@h, tail@t)` binds `head`→`h`.
  `field= name` means "destructure the field against pattern `name`", which is different.
- **Higher-order args include the nested parameter:** write `fn(v)` (with its parameter), not bare
  `fn`, in a definition that takes a function-typed argument.
- **Mixed-singleton lists don't unify:** `[True, False]` fails to type-check — `True`/`False` are
  distinct singleton types that don't widen to `Boolean` in one literal (Integer literals *do*
  widen). Use same-singleton (`[True, True]`) or single-element (`[False]`) lists in examples.
- **Rendering a polymorphic-element result is a frontier.** `sorted`/`reversed` return a list whose
  element type stays generic (`Ord`/existential), and the checker cannot reduce
  `List[Ord] ∩ List[ToString]` to the concrete element — so printing the result directly (which
  applies `toString`) fails to type-check. Render via comparison instead: `> (sorted(xs) == known)`
  prints `True`. A result with a *concrete* element (`range -> List[$T -> Integer]`) or a scalar
  (`sum -> Integer`) prints directly.
- **Element-preserving return types can trip the solver.** Returning `List[$T -> $_T]` (existential
  element) can push downstream `==`/`toString` into a "Grows by" scheme error. When it does, fall
  back to an erased return (`-> /Data/List`) and verify with structural `==`.
- **Formatting:** `stack exec catln -- fmt <file>` formats one file; `make ctformat` formats the
  whole repo (large diff — mention unrelated churn rather than committing it). `.ct` uses 2-space
  indent.

## 5. Process

1. Write the definition + its nested doc + a print-golden example together.
2. `stack exec catln -- run <scratch importing core>` to type-check fast, then
   `stack exec catln -- test <file>` to check the examples and property tests.
3. **Verify examples are actually checked, not skipped** — a print-golden on a non-String value is
   silently skipped. Corrupt a golden once and confirm it fails, or watch the passed-count rise.
4. Editing `stack/core/` churns goldens for every importing file; regenerate with
   `GOLDEN_TEST_WRITE=1 stack test` and confirm real pass/fail, not just the diff.
5. Finish Haskell-side changes with `stack build --pedantic` (`--fast` skips `-Werror`).
