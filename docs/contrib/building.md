## Building for development

To build the compiler, you can run `stack build --pedantic` from `$CATLN_HOME`.

Once it is built, you can execute the compiler on a Catln file by running `stack exec catln test/code/arith.ct`.

You can also use a repl as follows:

```
# Start stack repl
stack repl

# run command in repl
# the ... represents all files in the program
# process run the file
*...> process "test/code/arith.ct"
```

A repl that also keeps a stack trace for finding the source of errors can also be run.
Use `make errRepl` or `stack ghci --profile catln --ghci-options "-fexternal-interpreter -prof" --test`.
This can take a few hours to build the first time it is run.

### Tests

To run the Catln test suite, execute `stack test --pedantic`.

You can also load the tests within the repl:

```
# Start stack repl
stack repl

# Load the main test file in the repl
*...> :l test/Spec

# Execute the main test suite command including the full test suite
*...> main

# Execute the "arith" test located in "test/code/arith.ct"
*...> mt "arith"

# Execute the custom untracked test file located at "test/test.ct"
# This is useful during development
# Unlike the main tests, this one does not include the core library
*...> test
```

To rerun a property test, use:

```
stack test --ta '--hedgehog-replay "1:a2 Seed 10220680066336263475 12463056215188250787" -p "propCompactIdempotent"'
```

You can also use a retest similar to that defined in TypesTests from the REPL.

### Webdocs

Webdocs can be built using the standard way described in the [main building document](../building.md). In addition, another strategy for building webdocs can be done during development. It benefits from being somewhat faster to run, being runnable from the REPL without building code, and featuring live-reload for making changes to the webdocs.

In one command line, start the webdocs server using:

```
cd webdocs
npm start
```

Then, in another command line, run the webdocs API serving:

```
# See above for description of stack repl
stack repl
*...> :l test/Spec

# Run docs for test name "id" located in "test/code/id.ct"
*..> mtd "id"
```

Then, the API server should be available at `localhost:31204`.

The local testing site is available at `localhost:3000`.

### Formatting

As part of the development, we use [stylish-haskell](https://github.com/haskell/stylish-haskell) to ensure that code has a consistent appearance. The formatter can be run through:

```
make format
```

We also format the catln code using the catln formatter. This can be done with:

```
make ctformat
```

### Profiling

To profile, it can be done by running `stack test --profile`.
This produces the file `catln-test.prof`.

To make it easier to understand, there are tools to provide visualizations of the results.
One can be done by running `profiteur catln-tet.prof`.
This produces a file `catln-test.prof.html` which can be viewed.
