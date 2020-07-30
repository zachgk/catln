# Philosophy

This directory contains  a number of the core ideas and directives that drive the project. Many of these are not currently implemented but will hopefully be implemented in the future.

## Sub-Pages

- **[typeSystem](typeSystem.md)** - Discusses the basic elements within the type system.
- **[typeTheory](typeTheory.md)** - Introduction to the ideas behind the type theory.
- [testing](testing.md) - General introduction to testing philosophy and techniques
  - [arrowTesting](arrowTesting.md) - Automatic testing to ensure that overlapping function definitions should have the same result. It makes the use of multiple return values and implicit conversions safe.
  - [assertions](assertions.md) - Using assertions both for property testing and to declare properties.
- [Context](context.md) - An additional calling convention for passing data down through the call stack. This can be used for easier while still manageable state management.
- [genericTypeUsage](genericTypeUsage.md) - To fully support higher level classes, you must be able to create the higher level class and not just use it (e.g. get a list without specifying what kind of list).
- [macros](macros.md) - For a strongly typed language, you must either support unsafe features, require duplicate code, or support macros. This describes the macro usage within the language.
- [optimization](optimization.md) - Various aspects about performance optimization.
- [syntacticSugar](syntacticSugar.md) - Various forms of syntactic sugar to add.
- [Language Based Compilation](languageCompilation.md) - Move logic from the compiler to the language and library for greater capabilities, faster upgrades, more customization, and ease of use
- [compilerErrorStatement](compilerErrorStatement.md) - Declare error messages from within function definitions themselves.
- [documentation](documentation.md) - Documentation organization.
  - [naming](naming.md) - Thoughts on naming and some language specific guidance for good naming
- [modules](modules.md) - module and code scoping systems.
- [debugging](debugging.md) - Tools for debugging.
