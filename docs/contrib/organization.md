# Code Organization

The main executable is located in [app](app). The tests are located in [test](test). The rest of the code is located within the [src directory](src).

For understanding the code, it is best to start by looking at [Syntax](https://github.com/zachgk/catln/tree/master/src/Syntax.hs). This file contains main class definitions to describe the syntax of the program. The main piece of syntax is called `RawPrgm m` or `Prgm m` that includes some amount of additional metadata `m`.

Most of the code works similar to a pipeline. It converts the program code from one format to another and from one metadata type to another until the end result is finally used. The pipeline flows as follows:

1. [Parser](https://github.com/zachgk/catln/tree/master/src/Parser.hs) - `String -> RawPrgm PreTyped`. This parsers the syntax of the string text file into a raw format.
2. [Desugar](https://github.com/zachgk/catln/tree/master/src/Desugarf.hs) - `RawPrgm PreTyped -> Prgm PreTyped`. The desugar process removes various kinds of syntactic sugar and converts the code from the more user friendly external format into a more minimal format better suited towards optimization.
3. [TypeCheck](https://github.com/zachgk/catln/tree/master/src/TypeCheck.hs) - `Prgm PreTyped -> Prgm Typed`. The typechecking will execute typechecking and type inference to ensure that all types are known for all of the functions.
4. [TreeBuild](https://github.com/zachgk/catln/tree/master/src/TreeBuild.hs) - `Prgm Typed -> ResArrowTree`. Once typechecked, the implicit conversions are chosen and the code is formed into a single tree. This stage currently lives within the Eval stage as it requires knowledge about the primitive instructions and data types available.
5. [Eval](https://github.com/zachgk/catln/tree/master/src/Eval.hs) - `Prgm Typed -> Val`. The evaluator is an interpreter that will execute the code and produce final values. For building or compiling programs, it is executed through evaluation as a macro.
