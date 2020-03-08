# Philosophy

This directory contains  a number of the core ideas and directives that drive the project. Many of these are not currently implemented but will hopefully be implemented in the future.

## Key Ideas and Goals

Code has a dual nature. It both says what to do for the computer, but also what things are. In that respect, a program represents a summary of one's understanding about a problem domain.

The goal of a high-level language then should be to aid in describing that understanding. For example, types. While types can be used to determine that a value is an int, it can't represent what the value of that int is, whether it is prime, a power of two, or anything else. Becaues this information is know to the programmer but can't be expressed in the language, it results in defects that then require even more work to resolve.

One key example is getting the head of a `List[Int]`. Knowing nothing else, the result is an `Optional[Int]` because the list can be empty (where exceptions and null do not exist as they are ways to avoid sharing understanding through the language). Knowing more about the list such as the size is `5`, the return time should actually be just an `Int`. Any code that handles the no result case would actually deserve a warning as it is unreachable.

Inability to represent ideas can also lead to downstream effects. For example, Java has no good way of dealing with overlapping method definitinos. So, they can't support multiple inheritance because they wouldn't be able to resolve that problem. The lack of support means that they need to separate the code inclusion system and higher type system into classes and interfaces.

On the other side of it, being forced into oversharing is another problem. For example, if you need a list type then you might say it should be an array-based list. However, it is unclear whether this is done with intent because you have considered the options and decided that this is the right one. Or, are you just saying fairly thoughtlessly because you are required to specify the list implementation.

More specifically, consider the `sort` function. It exists in three dimensions:
- The formal, mathematical function, definition is a mapping from the domain of lists to the codomain of sorted lists. Each element in the domain goes to an element in the codomain. However, this format can't be represented for the most part.
- The second domain is the domain of algorithms. Here, you are able to choose between a few broad options such as QuickSort, MergeSort, SelectionSort, etc. Each of these algorithms should faithfully implement the mathematical function. For this reason, [arrow testing](arrowTesting.md) helps verify that they represent the same function.
- The third domain is the domain of details. For any algorithm, you can choose details such as lazy/strict, static/dynamic dispatch, parallelization and parallel scheduling, memory allocation and reuse, data types, etc. The details depend on the algorithm choice, but there are a larger number of them.

The goal is the language **proper** should exist entirely within the first and partially second domain. You should not even be able to descibe anything about which algorithm the function you are calling uses, and what the details are. This leaves all of these decisions up to the compiler to make. It should then be free to make those choices based on heuristics that are not obvious to the user, although there should at least be a mechanism for checking the decisions afterwards. Overall, you should only be able to discuss what the result should be, not how it is gotten.

Afterwards, it may be worth allowing users to specify those details through some tangential mechanisms. Maybe using compiler annotations to mark how to handle other code. Maybe in a separate file or language. The goal is that changing details should be completely isolated from the main code.

## Sub-Pages

- [arrowTesting](arrowTesting.md)
- [assertions](assertions.md)
- [compilerErrorStatement](compilerErrorStatement.md)
- [Context](Context.md)
- [genericTypeUsage](genericTypeUsage.md)
- [macros](macros.md)
- [optimization](optimization.md)
- [partialApplication](partialApplication.md)
- [syntacticSugar](syntacticSugar.md)
- [typeSystem](typeSystem.md)
- [typeTheory](typeTheory.md)
