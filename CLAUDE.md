# Catln

Catln is a term-rewriting programming language. The compiler is written in Haskell (GHC 9.6.x, Stack LTS 21.25).

**Compiler pipeline:** Parser (`String → RawPrgm PreTyped`) → Desugar (`RawPrgm PreTyped → Prgm PreTyped`) → TypeCheck (`Prgm PreTyped → Prgm Typed`) → TreeBuild (`Prgm Typed → ResArrowTree`) → Eval (`ResArrowTree → Val`)

## Editing Guidance

Remember that the compiler is still in development and there may be compiler errors during other tasks. Finding compiler errors is part of the purpose of tasks and should be highlighted.

Making changes to an early language must be done carefully and can have broad effects. Rely on the test suite to prevent regressions and try to keep changes small. When doing tasks and running into larger gaps, begin by investigating the cause of the gap and find specific evidence of how the language is deviating from expected behavior. Make plans citing the specific evidence. If there are multiple gaps make minimal reproducible examples to better isolate to single gaps and get those working while the main task is stashed. This helps make changes one at a time with an opportunity to get tests fully working and commit incremental progress in small commits.

Avoid making short-term fixes without express permission and instead focus on a long term approach intended to always work for all possible compiler inputs. Fallbacks that mask core failures should be avoided. When running into problems, find the root cause rather than fixating on the main goal. Making the functionality stable and reliable is more important than new features, and new features are a way to further exercise existing capabilities to find gaps and bugs.

Ask questions rather than making assumptions and investigate thoroughly. If you run into a problem and don't know how to figure it out, raise the issue with me. Provide direct evidence for all of the explanations with the artifacts from the code or outputs that led to the understanding         

## Build & Run

```sh
stack build --pedantic          # build
stack test --fast --pedantic    # run tests
stack exec catln <file.ct>      # run a catln file
make format                     # format Haskell code (stylish-haskell)
make ctformat                   # format Catln .ct/.ctx files
GOLDEN_TEST_WRITE=1 stack test  # update golden tests
```

Lessons that repeatedly cost time:
- **`--fast` does NOT enforce `-Werror`.** `stack test --fast` / `stack build --fast` compile `-Wall` findings as warnings, so unused bindings, incomplete patterns, etc. slip through and silently land in commits — then break `--pedantic` / `make hs-test` later. Always finish a Haskell change with `stack build --pedantic` (or `make hs-test`), not just `--fast`.
- **`make format` and `make ctformat` reformat the ENTIRE repo, not just your files.** They produce a large diff that may have unrelated `.ct`/`.ctx`/`.hs` files if an earlier commit failed to run the formatter. In this case mention it to me and leave the other formatting changes
- **Changes including anything in `stack/core/` can churn many golden test artifacts.** That churn is expected and helps ensure the changes are working as intended — regenerate with `GOLDEN_TEST_WRITE=1 stack test` and confirm real pass/fail by checking that individual tests' assertions still pass (a test can "fail" purely on a golden diff while its logic is correct).
- **A passing `GOLDEN_TEST_WRITE=1 stack test` is the gate — do NOT re-run the suite afterwards to confirm.** The write run executes every test's real assertions; rewriting the goldens only removes stale-diff noise. A second run just spends another ~10 minutes proving that files written from the run match the run. Review the golden diff instead.

## Code Organization

- `app/` — CLI entry point (`Main.hs`)
- `src/` — compiler library
  - `Syntax/` — parsing (Ct/ for Catln, Haskell/ for Haskell via tree-sitter, Md/ for markdown)
  - `Semantics/` — types, program representation, type graphs
  - `TypeCheck/` — constraint generation, type inference
  - `Eval/` — interpreter runtime
  - `CRes.hs` — compiler result/error monad
  - `MapMeta.hs` — metadata mapping utilities
  - `TreeBuild.hs` — arrow tree construction from typed programs
  - `LSP.hs` — Language Server Protocol support
  - `WebDocs.hs` — documentation server
- `test/` — test suites (Integration/, Syntax/, Semantics/, Typecheck/)
- `stack/` — Catln standard library
- `webdocs/` — documentation web UI (Node.js)
- `docs/` — language documentation

## Conventions

- `package.yaml` is the source of truth for build config (`catln.cabal` is generated — do not edit)
- Default extensions: OverloadedStrings, TupleSections, NamedFieldPuns
- `-Wall` is required — all builds use `--pedantic`
- Run `make format` before committing Haskell changes
- Run `make ctformat` before committing `.ct`/`.ctx` changes
- `.ct` files use 2-space indentation

## Writing `.ct` code (authoring gotchas)

- **Higher-order arguments must include the nested parameter.** Write `fn(v)` (its parameter), not bare `fn`, in a definition that takes a function-typed argument — see `test/Integration/code/ho.ct`. Bare `fn` fails with "Actual type contains bottomType".
- **To rename a destructured field, use the alias operator `field@newName` — not `field=newName`.** `Pair(first@f, second@s)` matches the `first`/`second` fields and binds them to the new names `f`/`s`. Writing `Pair(first= f, …)` instead tries to *destructure the `first` field's value against the pattern `f`* (wrong, and corrupts inference). The `@` takes **no space** (`first@f`, not `first@ f` — the parser expects the alias name immediately after `@`). A bare positional `Pair(first, second)` binds the fields under their own names.
- **Test `.ct` features in the same module where practical** (`stack exec catln -- <run|test> <file>`). A `stack/core/` function can type-check and evaluate correctly in-module yet fail when imported into another file.

## Testing

- **Framework:** Tasty + HUnit + Hedgehog (property-based)
- **Golden tests:** `test/Integration/golden/` (desugar/, tbuild/, typecheck/)
- **Integration test files:** `test/Integration/code/*.ct`
- **REPL testing:** `stack repl` → `:l test/Spec` → `mt "testname"` or `main` for full suite
- **Update golden tests:** delete specific golden files and rerun, or `GOLDEN_TEST_WRITE=1 stack test`

When fixing tests, fix type tests before looking at integration tests. The type tests are implemented through property tests. They should not be flaky because any failure indicates a hidden problem in the compiler. If one is encountered, keep and use the hedgehog replay seed to reproduce it. To ensure flaky tests are resolved, test with higher test count. Do not run the test suite multiple times to avoid the startup overhead. Instead, update the iteration count in TypesTests.hs and run the suite once with the larger count
