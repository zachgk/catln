# Code Organization

The main executable is located in [app](app). The tests are located in [test](test). The rest of the code is located within the [src directory](src).

For understanding the code, it is best to start by looking at [Syntax](src/Syntax.hs). This file contains main class definitions to describe the syntax of the program. The main piece of syntax is called `RawPrgm m` or `Prgm m` that includes some amount of additional metadata `m`.

Most of the code works similar to a pipeline. It converts the program code from one format to another and from one metadata type to another until the end result is finally used. The pipeline flows as follows:

1. [Parser](src/Parser.hs) - `String -> RawPrgm PreTyped`. This parsers the syntax of the string text file into a raw format.
2. [Desugar](src/Desugarf.hs) - `RawPrgm PreTyped -> Prgm PreTyped`. The desugar process removes various kinds of syntactic sugar and converts the code from the more user friendly external format into a more minimal format better suited towards optimization.
3. [TypeCheck](src/TypeCheck.hs) - `Prgm PreTyped -> Prgm Typed`. The typechecking will execute typechecking and type inference to ensure that all types are known for all of the functions.
4. Once typechecked, it can either be evaluated or compiled.
    a. [Eval](src/Eval.hs) - `Prgm Typed -> Val`. The evaluator is an interpreter that will execute the code and produce final values.
    b. [Emit](src/Emit.hs) - `Prgm Typed -> LLVMByteString`. The emit method will take the program and emit LLVM code to execute it. In the future, there will be more steps to perform various analyses for performance, parallelization, memory reuse, etc. before the Emit is called.
