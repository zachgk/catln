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
