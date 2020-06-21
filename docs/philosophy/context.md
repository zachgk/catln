# Context

One of the problems with functional programming is the way that state, especially global state, is handled. Imperative languages always have variables to store the state in. Often, mutable objects that are passes around and updated store the state, although that can make it difficult to understand where the state is changed. This can also be used in the case of singletons which are even more difficult to track.

In pure functions, the way that state is passed around is typically monads if not directly. However, these monads are difficult to combine if multiple states (such as logging, IO, counters, readers, etc.) are all needed. This has lead to the creation of an entire structure of monad transformers to bridge the gap.

For this reason, there will be an additional syntax known as Context. A method can require a context such as:
```
printAndLog{IO io, Log log}(String s) = ...
```

The context can be thought of as a set of values, each with it's own type. Values can be added to a scope and are then available within that context while inside that scope.

The context passes data down the call stack without requiring it to be specified at each level. This makes it convenient to have more global state. However, the state is clearly typed and lives within a well defined scope. I think this forms a reasonable compromise between the lack of global state in functional programming and the overabundance of global state in imperative programming.

Context can be used by specifying the values with a method. An element is pulled out of the context by specifying the type of the element. If multiple elements of that type exist within the context, it would result in a compiler error. For this reason, it may be common to create custom types just to store values for the context.

When a method accepts a context as part of it's definition, that values in that context are considered consumed. When the result of the method is returned, new context values (or the same ones) can then be returned alongside the return value as an update to the context.

It is also possible for values to not be included in the definition. For example, context values might be accessed by methods that are called, but not the main method. Those context values are referred to as covered. They do not need to be added to the context of the main method.

## Uses

The context can be used for various purposes. One example is that unlike Haskell, IO would be represented as a Context instead of a Monad. As an easy way to decide between Monads and Context, if you can have an unwrap method that discards the context and returns the value outside of the monad, it should be a context. Methods should return updated IO values when they accept IO as an input Context. Similarly to this, Context can be used for readers, writers, and state.

Context can also be used to pass constant environment information down the call stack. In this instance, the value would be consumed by functions that need it and the same value would be re-added to the context for the return value. It might be useful to add a `const` keyword as syntactic sugar for the reasonably frequent cases of values that are only read from the context.

Context could also be used to implement a listener or observer pattern. The listener event would be implemented as a type class. To add listeners, add values that implement the class into the context. The listener can be called by finding all values implementing the class within the context and calling the method on all of those values.

## Implementation

While Context will have it's own syntax, it can actually be implemented as a generic `Context` object in the standard library (although it would be a fairly complex one with a lot of macros). It is only a syntactic sugar. This means that the variable accesses can be fully determined during type inference. It is therefore a zero-cost abstraction.

For things such as failure state, they should not be represented using the context but by the return values. If it was represented in the context, it would be unclear which element in the context should contain the failure state.

It should also be possible for methods to be defined with default values to be used if the desired type is not found in the context. This can improve the generality of the method but still allow for contexts to affect it.

Alongside the context, it may also be helpful to have a syntax similar to the haskell "do" notation. The meaning of it can just be various forms of mapping like the original. But, it may need to support multiple kinds of maps simultaneously such as functor, monad, and also context monad map.
