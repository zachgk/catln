# catln

Catln is a tree-based programming language designed to be very high-level and very strongly typed.

**The language is currently under development. Only some of the critical language features have been implemented in the compiler so it is not possible to write code within the language yet. However, those interested can take a look at our [documentation](documentation) which describes in more detail the goals, philosophies, and implementation plans behind the language or message [@zachgk](mailto:zachary@kimberg.com).**

### High-Level

There are a wide variety of high-level languages that have different criteria for what high level means. The criteria I use is to separate information in a function into information that affects **what** the result is and information that affects **how** to get the result. The information affecting **what** include which subfunctions are called with what inputs. The information affecting **how** include what data structure implementations are used (array list vs linked list), memory types (stack vs heap), memory reuse, parallelization/threading, strict/lazy, static/dynamic dispatch, caching, etc. 

The goals for a high level language is to focus entirely on the question of what and make all of the details regarding how **impossible to express** in the language. The goal is to free the user from any possible concern or ability to affect performance while writing the code. They can focus only on essential complexity of their problem domain and avoid introducing additional complexity for performance. The compiler can then figure those performance details out itself.

For areas where performance is more critical, there will be supplementary annotations or configurations that can be used to specify information regarding the how question. Then, users can focus only on performance, easily try different things out, and be guaranteed that they can't introduce any bugs as they are not affecting what is returned.

### Strongly Typed

The other goal of the language is to have strong typing. Typing as well can have somewhat different meanings. In a low-level language, the typing exists closer to memory and is used to determine what registers and CPU instructions to use. Typing in some languages can allow you to introduce some abstractions to organize your code and type checking to help convert runtime errors into compile time errors. At higher levels, having the typing more closely reflecting the underlying problems can make it even more difficult to introduce bugs.

In this, the goal of a type is to represent things that you know before your code is run. While the actual values may not be known, some information about it can still be determined. Specifically, the type can be thought of as the set of all possible values and the goal is to narrow down the set as much as possible. For example, the set of integers is better than the set of numbers. But, the set of integers that are greater than or equal to zero is even better. This additional information can be used to determine when functions can be called, used to find compile errors, and used for optimizations. Once this is known, a programmer should try to represent everything they know about the program's problem domain using types. This information can be propagated through functions to achieve effects similar to theorem proving or formal verification. See [type system components](docs/philosophy/typeSystem.md) or [type theory](docs/philosophy/typeTheory.md).

### Tree Structure

The fundamental data structure in catln is a named tuple of the format `tupleType(argTypeA argNameA = argValA, argTypeB argNameB = argValB, ...)`. As the arguments can themselves be tuples, this forms a typed tree structure.

This format, like lisp, can be used to represent both code and data. For example, `addInts(Int left, Int right)` would be a tuple for the addition function. Something like `Point(Int x, Int y)` could represent a simple data type.

In addition to the data tuples, there are also arrows which convert one tuple format to another. For example, there could be an arrow `addInts(Int left, Int right) -> Int` that applies the integer addition operation. These arrows can match complicated patterns involving the data themselves, and even patterns involving the composition of multiple functions. Arrows can even take the same input tuple to multiple output tuples. See [more](docs/philosophy/typeSystem.md).

That leaves the question of when arrows should be applied. Lisp relies on quoting and unquoting to convert between lists and functions. In Catln, a program that is well-typed means that any order of applying the arrows should result in the same values for the same types. This property is verified by [arrow testing](docs/philosophy/arrowTesting.md). Therefore, the actual choice of order of application of arrows is left as a **how** question to be determined during compilation. See more in the [type system components](docs/philosophy/typeSystem.md).

## Learn More

You can learn more by checking out:

- [Documentation](docs)
  - [Project Goals and Ideology Documentation](docs/philosophy) - These documents describe different interesting advancements in the language including everything above, modules, testing, documentation, debugging, and more language features.
- [Compiler Test Cases (as Catln code examples)](test/code)
- [Getting Started Instructions](docs/gettingStarted.md)

Contact [@zachgk](mailto:zachary@kimberg.com) if you have any thoughts, ideas, questions, or feedback about the language.
