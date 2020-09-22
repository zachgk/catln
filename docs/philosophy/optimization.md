# Optimization

It is typical for languages and their respective compilers to be able to perform various optimizations. These optimizations allow users to write code that is more focused on readability than performance, while still gaining the equivalent performance. It can also greatly simplify code by reducing the amount of work necessary to implement various optimizations.

One key focus of this language which differs from others is that optimizations should not be predictable. For example, consider strict (data is always computed immediately) vs lazy (data is computed only when it is needed). Most languages are predictable whether they will implement something lazily or strictly. Often, this is done by only supporting one of the two (C is always strict) or by requiring a special indication to deviate from the standard (Haskell is lazy unless told otherwise). The result of these simplistic decisions is that very often users must accept the burden themselves. C users have to manually implement laziness any time in which it could improve work. But, it is more likely that they simply would accept the easier and worse performing option of not bothering to implement it. In Haskell where users can specify a choice, it usually results in duplicate implementations where one is strict and the other is lazy. Users must therefore choose between the two implementations and the doubled maintenance costs.

The solution to this problem is that some heuristic should be implemented to decide between the options in this use case. Any simple heuristic is likely to be bad so the only result is to use a complicated heuristic that users would not be able to easily predict the result of. Given this, users would have to check the result in order to determine what the heuristics resolved. This process could be eased with a dedicated compiler query that would return the results of this heuristic. Users trying to optimize performance could then look at the results and then change the values that they don't agree with compiler annotations. The heuristics could also be configurable to determine how much to prioritize competing goals such as performance and binary size. These could be used to tune the output results to better suit the specific application needs.

One requirement to improve these heuristics is that users must be able to express large amounts of information about their computations. For this reason, the ability to express metadata about computations should be a focus of the syntax and the implementation. These include both rigid bounds (this number must be less than 128) and flexible bounds (the list usually has a small number of elements).

Below, I will discuss a number of these kinds of optimizations that should be implemented in order to avoid passing on the implementation cost to the users writing in the language. Note that the focus of this is high level optimizations. It is assumed that the language will be converted into a lower-level IR and be passed through another optimization system (currently LLVM) that applies unmentioned lower-level optimizations as well.

## Inlining

One of the most basic examples of this case is inlining. If you inline a function, it will increase the performance by removing the calling overhead. When the function is only used in one place, it also will reduce the binary size a bit. If the function is inlined into multiple places, it would increase the binary size by replicating the function in multiple places. It is most beneficial when the function is small and the calling overhead consists of a large amount of the total cost of the function. When it is large, the calling overhead makes little difference. Another potential criteria is that it can make optimization more difficult for the downstream compiler because larger functions require more work to optimize.

## Strict vs Lazy

Results of computation can either be strict or lazy. If they are lazy, they are computed only when needed which can save the overhead of computing values that are not needed. When those values require significant computing time, this could be a large savings. It can also be used in two additional ways. It can actually change the algorithmic complexity of a computation such as the famous example of quicksort with laziness (O(N^2)) being equivalent to quickselect (O(N)). Lastly, it can be used in the case of infinite computations such as infinitely sized lists. These can then be used to simplify other computations and have spread outside of just the functional world such as python generators.

Strictness also has a number of benefits. It has no overhead in terms of performance cost or memory usage while laziness does. This can be an especially large problems in cases such as inserting elements into a map where there are large numbers of small functions that could all be deferred. Using strictness will reduce the overall performance of the application by a significant margin. For this reason, strictness should be considered the default behavior and laziness only used when the benefits outweigh the costs.

Laziness should be used whenever something has infinite size. In the case of a conditional where some values are only used by one branch of the condition, those values should be made lazy if they require a non-trivial amount of work to compute (greater than laziness overhead). Note that this also applies to the properties of an object where some properties could be lazy while the whole is not. As another potential improvement, values can be lazy in tandem where multiple will all be accessed or none will be accessed. Combining those could further decrease the performance overhead.

## Caching

Another important optimization is the use of caching to prevent values from being recomputed. However, caching also has an overhead in terms of performance (checking if a value was cached) and memory usage (storing a potential value). It also greatly increases the memory usage by preventing the cached values from being freed.

One potential use of caching is associated to objects. For example, the length of a linked list could be associated with it. If the length is commonly queried, then it makes more sense to cache it. This would also mean computing it recursively as the linked list is created. Storing the cache as part of an object can help for determining memory usage because the cached values share the same scope with their associated object.

Another important area of caching is general memoization. Consider an incremental compiler as an example. Using memoization, it would be trivial to avoid recomputing operations that are isolated such as removing syntactic sugar from a function or typechecking it. With memoization, this performance improvement comes for free as long as the compiler runs as a daemon or server where the memory can be preserved. Otherwise, it requires specific intention to use.

Another more specific use case is dynamic programming. This involves a recursive function (or mutually recursive functions) that combine to compute a single value. It can be improved using standard memoization, but specific handling can result in better memory usage. It is also worth noting that some of these dynamic programming problems are also universal (such as fibonacci) and could be reused, so it shouldn't be local only.

## Unused Values

For an object, it is possible that the values on it can be unused. For example, a linkedlist can contain a head pointer, tail pointer, forward pointers, and backward pointers. Depending on how it is used, it is quite possible for some of these pointers to not ever be needed. In that cases, those pieces of data can be pruned from the object to remove the memory usage and computation cost.

Another potential avenue for this is values that can be determined at compile time. For example, the length of hashsum can be known based on the type of sum and neither needs to be stored nor computed.

## Remove levels of Abstraction

It is often useful to have wrapper types such as Optional which help enable sum types. In this case, it should not have a cost to use these types over not having them to avoid discouraging good usage of abstraction. For example, if the optional is guaranteed to return a value, it should be equivalent to not having an optional at all.


## Type Selection

Another potential area for optimization is the selection of types. For example, users must pick between the different list variations including linked lists, array lists, skip lists, deque, trees, and hashmaps. Languages then have to choose between which implementations are worth supporting and how many can be supported before it merely confuses users. Some languages simply provide one option with the idea that it is sufficient.

For this reason, it should be important to allow the compiler to choose between a number of implementations of an interface. This would also require some way for the implementer to provide guidance such as avoid this implementation if it calls this function. It can also take advantage of profiling through the unit and integration tests to determine how the interface is typically used. Users could also specify an actual value. But, even a poor heuristic from the compiler is likely to be better than users choosing without any particular thought. See [more](choice.md).

## Method specialization
When there are multiple possible implementations of a function, there may not always be a best option. For example, sorting with quicksort is better for large arrays while sorting with selection sort can be faster with small ones. This can be used to improve the overall performance.

## Memory Management
Memory management is one of the most important optimizations in a language. Systems generally fall into manually managed memory (C++, Rust), Garbage Collection (Java), and Reference Counting (Swift, Python). While having memory managed by systems like this can greatly reduce bugs and simplify development, that is only if it works well enough. A bad management system does not leave you any better off.

For memory management, the primary system should be passed on lifetime. Consider your functional program as a tree. Values are created and can then be propagated up the tree. However, there should exist a level on the tree where the value is guaranteed to be removed and propagated up no farther. By doing this analysis, the values can be freed without requiring any runtime analysis and they can be allocated and freed efficiently in a combined group as well. This technique can be supplemented by reference counting as well.

Another component for analysis is the use of stack vs heap. Using the stack can help improve the memory usage as well when it is possible.

## Parallelization
One of the key benefits of functional programming is that it can greatly simplify parallelization. By having pure code, it guarantees that sections must be independent and therefore can be computed independently in different threads. However, diving the work up into the threads requires a little bit of work. Functions should be divided up into chunks which can each be run in parallel. Then, they can be added into a global queue and multiple lightweight threads can each pull elements off the queue and then execute them. If chunks depend on other chunks, they will check if all chunks have finished after working and then add the successive chunk code to the queue at the end.

Chunks should be large enough to avoid contention for the queue and not spend too much time outside of chunks. Then can include one or several subtrees that will be arguments to a larger tree. For large parallel operations (like a map), they should be divided into chunks with a fixed size that is determined based on the code in the map and then the remainder can be combined with the last chunk.

While this works for thread parallelization, there is also GPU style parallelization. For this, it can be focused on specific data types that typically work well with GPUS. It may also require a special context to enable GPU support as well.

## Only subset of Function needed
Sometimes, the application may not need all of the functionality provided by a library. For example, if it only calls the function with one argument set to true, it would be more optimal to remove that argument from the function and substitute the value into it. This requires additional compilation but has only benefits during runtime.

## Promises and Futures
One area to consider is how to manage asynchronous code including promises and futures. For most of the code which should be pure, this shouldn't be a problem because the general parallelization should be able to handle it. Some structure may be needed for IO and the monads that are order dependent.

## Static and Dynamic Dispatch
One avenue worth considering is static vs dynamic dispatch. Static dispatch can be faster although it requires duplicating the code for each of the possible input types (binary size). Dynamic dispatch requires another layer of indirection before making calls but does not duplicate methods. The best option might be a combination of the two depending on the requirements and the goals. For example, only use static dispatch for the hot loops where it would greatly increase performance. There may also be hybrid systems that would further reduce the cost.
