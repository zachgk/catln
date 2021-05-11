## Building for development

To build the compiler, you can run `stack build` from `$CATLN_HOME`.

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

To run the Catln test suite, execute `stack test`.

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
