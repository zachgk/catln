# Catln

Catln is a term-rewriting programming language. The compiler is written in Haskell (GHC 9.6.x, Stack LTS 21.25).

**Compiler pipeline:** Parser (`String → RawPrgm PreTyped`) → Desugar (`RawPrgm PreTyped → Prgm PreTyped`) → TypeCheck (`Prgm PreTyped → Prgm Typed`) → TreeBuild (`Prgm Typed → ResArrowTree`) → Eval (`ResArrowTree → Val`)

## Build & Run

```sh
stack build --pedantic          # build
stack test --fast --pedantic    # run tests
stack exec catln <file.ct>      # run a catln file
make format                     # format Haskell code (stylish-haskell)
make ctformat                   # format Catln .ct/.ctx files
GOLDEN_TEST_WRITE=1 stack test  # update golden tests
```

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

## Testing

- **Framework:** Tasty + HUnit + Hedgehog (property-based)
- **Golden tests:** `test/Integration/golden/` (desugar/, tbuild/, typecheck/)
- **Integration test files:** `test/Integration/code/*.ct`
- **REPL testing:** `stack repl` → `:l test/Spec` → `mt "testname"` or `main` for full suite
- **Update golden tests:** delete specific golden files and rerun, or `GOLDEN_TEST_WRITE=1 stack test`
