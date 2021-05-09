# Getting Started

## Prerequisites

There are two main prerequisites for installing Catln:
- [Haskell GHC comiler](https://www.haskell.org/ghc/distribution_packages.html)
- [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/), to build the compiler and manage Haskell dependencies.

## Installation

To install the Catln compiler, begin by checking out the Catln repository. I will refer to this directory as `$CATLN_HOME`. Then, you can install it by running:

```bash
cd $CATLN_HOME
stack install
```

This installs Catln to `~/.local/bin/catln`. You may have to add `~/.local/bin` to your path in order to run catln. Due to current limitations, the catln compiler must be run when your current directory is `$CATLN_HOME` in order to find the catln stack and core library.

You can determine the available options for catln by running `catln --help`.

If you want to run the docs, you will also need to build the docs site as well. Install `npm` and then run the following:

```
cd $CATLN_HOME/webdocs
npm install
npm run build
```

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
