# Getting Started

## Prerequisites

There are three main prerequisites for installing Catln:

- [Haskell GHC comiler](https://www.haskell.org/ghc/distribution_packages.html)
- [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/), to build the compiler and manage Haskell dependencies.
- LLVM 8 which can be installed through a package manager such as `brew install llvm@8` or `apt install llvm-8`

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

## Running

Now that it is installed, the compiler should be available in your path. You can test this by running `catln --help`.

There are several subcommands available within the compiler.

You can use `catln run FILEPATH [FUN]` to run a particular file. It will expect to find a definition of `[FUN]` within that file to run. Passing `[FUN]` is optional and the default is the function named `main`. This command must be run from the directory of `$CATLN_HOME`.

You can use `catln build FILEPATH [FUN]` to build a particular file. It will expect to find a definition of `[FUN]` within that file defining what to build. Passing `[FUN]` is optional and the default is the function named `main`. This command must be run from the directory of `$CATLN_HOME`.

You can use `catln doc PATH` to run the doc server on port `8080`. It will serve all `.ct` files located recursively in the `PATH` if it is a directory, or just `PATH` if not. It will also include all dependencies in the doc build. This command must be run from the directory of `$CATLN_HOME` and requires some additional installation (see above).

There are also several additional options to run in the [building for development document](contrib/building.md).
