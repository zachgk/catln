# Context

One of the fundamental problems for a programming language is the management of state. In order to manage state well, there are a few goals for a successful system.

In imperative languages, the state management is relatively straightforward by using variables and memory. However, it behaves like a blunt instrument in that it works for everything but loses too much information. It is hard to know what kind of variables, mutations, and state changes could happen in a function. It also restricts the compiler due to the limits of analyzing the control flow with the singular state. The first goal is that the state should be explicitly controlled in the type system so it is easy to understand and optimize.

In functional languages, there isn't an implicit variable map and states have to be created explicitly. This does make programs easier to understand, but it can be verbose and unwieldy to have different values (state1, state2, state3, ...) for every change made to a state. This brings the second goal that state should be easy to update.

The first technique functional programming languages use to manage the state, beyond the naive, is as a monad. Combined with a "do-notation", this saves significantly on the verbosity. However, the monad is fairly unintuitive for a full state. It converts your function into a lambda that is run using a `runState` function. This means that when combining functions, you are building up a larger and larger function to execute. This is remarkably convoluted. The simpler form of a state is that a function should accept a state as part of the input and return a state as part of the output. The third goal is that state management should be intuitive.

The other issue with monads is that they are difficult to combine. If each monad fulfills the idea of "do one thing and do it well" instead of having a monolithic monad, then it must be possible to combine the monads. Unfortunately, monads can't combine. Building monoliths is also not right. If using the slightly more powerful form of a monad transformer, they can combine by it is complicated, verbose, and ultimately lacking.

More than just combining, the combination needs to change easily. You need to add things to the state partway through the computation or prune state when it is no longer needed. This shouldn't be a side part, but one of the key aspects of state management.

One approach which I have seen in more recent systems is effect systems. This is similar to monads, but adds more into combination and a bit into adding/removing parts of state. However, it also falls into the problem that it is based on monads instead of state changes (`$A -> $M<$B>` but should be `$M<$A> -> $M<$B>`) as being convoluted.

## Context

The solution that Catln uses is called context. It corresponds to a data object defined in the standard library:

```
data Context<$T>($T val, $states...)
```

The context can be thought of as a collection of values, each with it's own type. The primary operations on the context are to add additional elements of state to the context and to get the elements of the state from the context. The elements in the context are all values that have types, and can be identified by the type.

In addition, there are also some syntax sugars to make it easier to work with the context. Many methods will require a context to be called such as:

```
# The println method requires the IO object to be part of the context when it is called
# It then returns nothing (unit) inside of an updated context object
println{IO io}(String s) -> {IO}()

# It corresponds to the desugared form:
Context(val=println(String s), IO $io, $states...) -> Context(val=(), IO $newio, $newstates...)
```

Within the println function, including IO inside of the curly braces means to remove the IO element from the context. Then, the new IO produced after the println operation has to be re-added to the context before returning.

Many functions will use context elements indirectly. They do not need to access the values in the context, but they call functions that need to access values inside the context. For these, no changes to be made to the parent function. While this may seem impure in that it gives functions side effects, it really just saves a bit of typing. The true context requirements of the function and the transitive requirements are computed during type inference, so they can be displayed by the IDE or in the docs (which is really all you need).

There is also a few additional syntax sugars for contexts:

```
# A value can be prepended with the context to add something to a context
# This is very useful when returning while adding context values
x = {valToAddToContext} valInsideContext

# The standard context get requires that exactly one element of that type should be in the context
# Otherwise, it will throw a syntax error during compilation
# This syntax stores all elements as a Collection<MyListenerType> which can be zero or many listeners
callWithListeners{MyListenerType... listeners}(...) = ...

# Add an element to the context within a scope
# This makes it available within the block and removes it when the block ends
foo =
  with {newContextElement}
    ...
```

## Uses

The context can be used for various purposes. One example is that unlike Haskell, IO would be represented as a Context instead of a Monad. As an easy way to decide between Monads and Context, if you can have an unwrap method that discards the context and returns the value outside of the monad, it should be a context rather than a monad. Methods should return updated IO values when they accept IO as an input Context. Similarly to this, Context can be used for readers, writers, and state.

Context can also be used to pass constant environment information down the call stack. In this instance, the value would be consumed by functions that need it and the same value would be re-added to the context for the return value. It might be useful to add a `const` keyword as syntactic sugar for the reasonably frequent cases of values that are only read from the context.

Context could also be used to implement a listener or observer pattern. The listener event would be implemented as a type class. To add listeners, add values that implement the class into the context. The listener can be called by finding all values implementing the class within the context and calling the method on all of those values.

## Implementation

While Context will have it's own syntax, it can actually be implemented as a generic `Context` object in the standard library (although it would be a fairly complex one with a lot of macros). It is only a syntactic sugar. This means that the variable accesses can be fully determined during type inference. It is therefore a zero-cost abstraction.

For things such as failure state, they should not be represented using the context but by the return values. If it was represented in the context, it would be unclear which element in the context should contain the failure state.

It should also be possible for methods to be defined with default values to be used if the desired type is not found in the context. This can improve the generality of the method but still allow for contexts to affect it.

Alongside the context, it may also be helpful to have a syntax similar to the haskell "do" notation. The meaning of it can just be various forms of mapping like the original. But, it may need to support multiple kinds of maps simultaneously such as functor, monad, and also context monad map.
