# Type Theory

## Existing Systems

In most functional languages, the type theory is based on lambda calculus. The idea is that a function accepts a single type and then returns a new type. Functions of multiple types can be represented through currying, that it accepts the first type and then returns a new function that requires one fewer types to return the final result. This makes it naturally well suited to higher order functions.

The problems occur once the type theory starts adding additional ideas. For sum types, they are handled by creating functions such as `Leaf :: v => Tree v` that create the sum types. While this works in general, it also hides a number of limits. It makes it difficult to handle method overloading and default arguments in methods. Product types can be handled by creating tuples. However, the result of this is that there exists the conversion of currying: `a -> b` vs `(a,b)`.

The other major extension is adding generics. They are typically considered as existential qualilfiers similar to `∀a. a -> a`. However, this has now introduced an entire dimension of kinds into the type system for methods.

## Catln

Catln is based on a directed graph structure. The nodes are named tuple trees (see [type system](typeSystem.md)).

The edges of the graph represent implicit conversions. It is inspired by [Scala](https://docs.scala-lang.org/tour/implicit-conversions.html). The idea of an implicit conversion is that it represents a conversion that has a clear definition such that it can happen automatically. The clearest example is from a function tuple to applying that function. But, you could also automatically convert between types such as between different types of lists (linked list -> array list).

Which implicit conversions should be taken and when should be found during type inference. The functions called on values serve as hints for when implicits are necessary.

However, this can also result in problems if conversions happen unexpectedly or it converts to the wrong type. In theory, a well typed program should produce the same result regardless of the order of implicits as long as the implicit combination allows you to reach the desired type. This property is guaranteed through the use of [arrow testing](arrowTesting.md). Therefore, most issues that implicit conversions can cause should be found and they can generally be assumed to be safe.

This also makes handling more complex language features quite simple. Each arrow definition can correspond to many possible edges in the type graph. First, it can match multiple values for a single argument such as an integer argument. Arrows can also match classes or multiple arrows for each object within a class can be equivalent to an arrow for the entire class. Even type properties can simply be considered as nothing but arrows that apply to sets of nodes. The arrow definition can even match up to a generic. Lastly, arrows can also substitute to be applied to a subtree of the main tree at a single node in the graph. So, all fancy language features essentially convert down into the two simple representations of objects and arrows.

The language has it's current (working) name because it was inspired by category theory. While products are usually referred to as integrated into the nodes in the graph, the type classes behave similarly to sums within the graph. Each definition represents a commuting relationship in the graph. The arrow testing requirement that all orders are equivalent is essentially the requirement that all possible combinations of the commuting definitions combined should commute.
