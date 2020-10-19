# Catln

Catln is a new paradigm of programming languages based around the idea of implicit conversions.
The language focuses at a high level on the expression of ideas.
It can encode ideas that are impossible to describe in most other languages and avoids low-level ideas that other languages leave you no choice but to express.
It is most similar to the functional programming languages, but is not technically based around functions.

## Language Summary

There are two key pieces to Catln: objects for data and arrows for operations.

### Objects

The fundamental data structure in catln is a named tuple of the format `tupleType(argTypeA argNameA = argValA, argTypeB argNameB = argValB, ...)`. As the arguments can themselves be tuples, this forms a typed tree structure.

This format can be used to represent both code and data. For example, `addInts(Int left, Int right)` would be a tuple for the addition function. Something like `Point(Int x, Int y)` could represent a simple data type.

With this key structure, it allows for a simple and powerful definition of types: a set of named tuples. Any type feature, no matter how complicated, simply reduces into these sets making them easy to use and reason about.

Types can be combined. By taking the union of types, it can represent all the power of many features including enums, sum types, inheritance, and type classes.

Types can also intersect by using type properties like `List_sorted(True)_length<Int_gt(5)>`. The intersection allows for more information about types to be expressed and inferred through type inference. It can then be used for purposes such as optimization or even simple formal verification of [assertions](philosophy/assertions.md). [See more about the type theory here](philosophy/typeTheory.md).

### Arrows

While objects support data and functions, arrows enable behavior. Each arrow matches an input type and converts it into an output type such as `addInts(Int left, Int right) -> Int`. A function definition would create both an object to build the function call and an arrow to execute it.

The arrows are very flexible. They can have overlapping input types such as `sqrt(Num val) -> Optional<Num>` and `sqrt(Num_gte(0) val) -> Num`. It can even produce a completely different output type from the same input such as `sqrt(Num val) -> Complex<Num>`. Arrows can also match patterns that have multiple levels such as `++(String left, right=++(left=rl, right=rr)) = concat([left, rl, rr])`. [See more about arrows](philosophy/typeSystem.md).

That leaves the question of when arrows should be applied and which arrows should be applied. Arrows are applied automatically by the compiler without any specific call by the user. The arrows which are valid in any particular location are determined during type inference based on the inputs and outputs. This leaves the question of which valid arrows to apply for the best performance. Essentially, this makes the programs produced abstracted over the choice of arrows.

The goal of deferring the choice of arrows is to only allow you to express information that describes **what** the result of a function should be, but make it impossible to express information about **how** to get the result. This provides a clear separation of concerns so you can focus on one level of abstraction at a time. Later, you can provide heuristics and manual overrides to address the question of how by determining these arrow [choices](philosophy/choice.md). Similarly, you can also abstract over other **how** [choices](philosophy/choice.md) such as what approximation algorithm or data structure to use. Information about [performance optimization](philosophy/optimization.md) works similarly.

One possible concern is that following different order of applying arrows can result in unexpected behaviors. A well-typed catln program should have any order of applying the arrows result in the same values for the same types. Essentially, different arrows for the same input should be different algorithms and expressions for the same mathematical function. Instead of trusting the assertion, it can actually be tested automatically by using [arrow testing](philosophy/arrowTesting.md). Not only does this allow users to trust the arrow process, but it also provides free testing and code coverage that users don't need to write manually.

### Miscellaneous

Here are some of the other highlights for features which are possible in the language:

- Catln has a different strategy of code organization that is intertwined with documentation. Imagine you were writing a book to teach someone about your code. You want to organize it in the clearest way to explain the concepts to someone else. Now, imagine you want your book to precisely describe the code. The best way to do that is to include all of the code in the book. At this point, your "book" is how a code base should look. The purpose for this is to include commentary about not just what code you have, but why you wrote the code you did. [See more about documentation](philosophy/documentation.md).
- In order to organize code, Catln has modules. When you use a function, you can avoid describing the full module path and instead infer which module your function should be from as part of standard type inference. This means no need for named or qualified imports and large numbers of values can be imported directly into the same scope. It also helps simplify [naming](philosophy/naming.md). [See more about modules](philosophy/modules.md).
- One issue with functional languages is managing state and propagating information tediously down the call stack. While monads can work, combining different monads for different kinds of state adds unnecessary complexity. Catln adds a standard `Context` type that automatically passes various kinds of state down the call stack. It can be used for semi-global constants, IO, logging, counters, or even event listeners. [See more about Context](philosophy/context.md).
- While Catln can be used to program a normal exectuables, it should be able to describe ideas which are larger than a single executable. For example, it could build both a web client and server, a distributed system, or even an entire cloud architecture with a CloudFormation template. This let's the type checing ensure that all levels of your program work together properly and eliminates bugs. This is best done by moving the compilation and optimization process from the compiler and instead implement it within the standard library using the powerful meta-programming features Catln provides. [See more about language compilation](philosophy/languageCompilation.md).

While these cover some of the most interesting ideas of the language, many more ideas as well as many further details can be found within the [project goals and ideology documentation](philosophy).

**The language is currently under development. Only some of the critical language features have been implemented in the compiler so code written in the language is still somewhat limited.**

Contact [@zachgk](mailto:zachary@kimberg.com) if you have any thoughts, ideas, questions, feedback, or concerns about the language. I am also looking for collaborators so let me know if you have any interest in working on the language with me.
