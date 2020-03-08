# Type Theory

## Existing Systems

In most functional languages, the type theory is based on lambda calculus. The idea is that a function accepts a single type and then returns a new type. Functions of multiple types can be represented through currying, that it accepts the first type and then returns a new function that requires one fewer types to return the final result. This makes it naturally well suited to higher order functions.

The problems occur once the type theory starts adding additional ideas. For sum types, they are handled y creating functions such as `Leaf :: v => Tree v` that create the sum types. While this works in general, it also hides a number of limits. It makes it difficult to handle method overloading and default arguments in methods. Product types can be handled by creating tuples. However, the result of this is that there exists the conversion `a => b` vs `(a,b)`.

The other major extension is adding generics. They are typically considered as existential qualilfiers similar to `∀a. a => a`. However, this has now introduced an entire dimension of kinds into the type system for methods.

## Flng

Flng is based on a directed graph structure. The nodes are named tuples that can either be a sum or product type. As an example, `Tree = Node(Value v, Tree left, Tree right) | Leaf(Value v)`.

The edges of the graph represent implicit conversions. It is inspired by [Scala](https://docs.scala-lang.org/tour/implicit-conversions.html). The idea of an implicit conversion is that it represents a conversion that has a clear definition. Which implicit conversions should be taken can be inferred based off the methods that are called on the result of the conversion.

However, this can also result in problems if conversions happen unexpectedly or it converts to the wrong type. This problem is solved through the use of [arrow testing](arrowTesting.md). Arrow testing guarantees that whatever implicit conversions are made, the results of the computation should be the same so there is no reasonable way to mess up.

There are a number of powerful results from using implicit conversions. First, functions can be represented using the combination of a tuple and an implicit conversion. Unifying the ideas of a tuple and function first greatly simplifies the complexity of the language. It also unifies many of the problems that they both face such as currying in functions and builders in objects.

Another benefit is that it allows definitions of functions with function inputs. For example, `map(map(x, f), g) = map(f(g(x)))` could be written. It only uses map as a tuple so that the tuple is passed to the other map. During the optimization, that would be simplified to the simple composition. This can also be used in other cases such as the concatenation of three strings is equivalent to a join (which could result in poor performance in other languages).

### Generics 

After this, one of the biggest questions is handling generics. The way it should be handled is that the nodes and arrows in the directed graph all represent primitive operations of fully defined types. If methods are defined using types, it should be represented as a set of arrows. In fact, it is easy to represent all further degrees of complexity in terms of (potentially infinite) sets of nodes and arrows.

Type properties can also just be represented as sets of nodes and arrows. There would also be an automatic "forget property" implicit that would remove properties which are not useful. Properties can also utilize the arrows to convert something like `Int_GT(5) => Int_GT(0)`. This allows for a simpler handling of properties by downgrading from a more specific property to use methods defined in terms of the lesser property.
