# Getting Started

## Installation

First, [install Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) in order to run the language. Stack will run your build and manage the dependencies.

## Building and Running

To build the compiler, you can run `stack build`.

Once it is built, you can execute the compiler on a Catln file by running `stack exec catln-exe test/code/arith.ct`.

You can also use a repl as follows:
```
# Start stack repl
stack repl

# run command in repl
# the ... represents all files in the program
# processFile returns the process exit value (0 for no errors)
*...> processFile "test/code/arith.ct"
0
```

### Tests

To run the Catln test suite, execute `stack test`.

You can also load the tests within the repl:
```
# Start stack repl
stack repl

# Load the main test file in the repl
*...> :l test/Spec

# Execute the main test suite
*...> main

# Execute the "arith" test
*...> mt "arith"

# Execute the custom untracked test file located at "test/test.ct"
# This is useful during development 
# Unlike the main tests, this one does not include the standard library
*...> test
```
