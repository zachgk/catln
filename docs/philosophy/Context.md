# Context

One of the problems with functional programming is the way that state, especially global state, is handled. Imperative languages always have variables to store the state in. Often, mutable objects that are passes around and updated store the state, although that can make it difficult to understand where the state is changed. This can also be used in the case of singletons which are even more difficult to track.

In pure functions, the way that state is passed around is typically monads if not directly. However, these monads are difficult to combine if multiple states (such as logging, IO, counters, readers, etc.) are all needed. This has lead to the creation of an entire structure of monad transformers to bridge the gap.

For this reason, there will be an additional syntax known as Context. A method can require a context such as:
```
<IO io>println(String s) = ...
```

The context is identified by it's type if only one object of that type can be found at a time. If necessary, it can help to create new types just to wrap around an existing type to specify the purpose such as a `VarNameCounter(Int)`. 

When a method accepts a context as part of it's definition, that context is considered consumed. A new context (or the same one) can then be returned alongside the return value as an update to the context.

While this is the behavior of a context, it can just be desugared into a single unified Context monad with variable number of type parameters. This also means that each of the accesses of the monad must be discerned statically during the desugaring or typechecking process.

For things such as failure state, they should not be represented using the context but by the return values. If it was represented in the context, it would be unclear which element in the context should contain the failure state.

It should also be possible for methods to be defined with default values to be used if the desired type is not found in the context. This can improve the generality of the method but still allow for contexts to affect it.

Alongside the context, it may also be helpful to have a syntax similar to the haskell "do" notation. The meaning of it can just be various forms of mapping like the original. But, it may need to support multiple kinds of maps simultaneously such as functor, monad, and also context monad map.
