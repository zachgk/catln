# Philosophy

This directory contains  a number of the core ideas and directives that drive the project. Many of these are not currently implemented but will hopefully be implemented in the future.

## Sub-Pages

- [typeSystem](typeSystem.md) - Discusses the type system used and various features including class, type properties, multiple return values, and multiple level functions.
- [typeTheory](typeTheory.md) - Introduction to the basic type theory used.
- [arrowTesting](arrowTesting.md) - Automatic testing to ensure that overlapping function definitions should have the same result. It makes extensive use of multiple return values and implicit conversions safe.
- [assertions](assertions.md) - Using assertions both for property testing and to declare properties.
- [compilerErrorStatement](compilerErrorStatement.md) - Declare error messages from within function definitions themselves.
- [Context](Context.md) - An additional calling convention for passing data down through the call stack. This can be used for easier while still manageable state management.
- [genericTypeUsage](genericTypeUsage.md) - To fully support higher level classes, you must be able to create the higher level class and not just use it (e.g. get a list without specifying what kind of list).
- [macros](macros.md) - For a strongly typed language, you must either support unsafe features, require duplicate code, or support macros. This describes the macro usage within the language.
- [optimization](optimization.md) - Various aspects about performance optimization.
- [partialApplication](partialApplication.md) - Calling a function by providing the argument in multiple stages.
- [syntacticSugar](syntacticSugar.md) - Various forms of syntactic sugar to add.
- [documentation](documentation.md) - Discusses how documentation should be written.
- [debugging](debugging.md) - Tools for debugging.
