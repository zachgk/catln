# Type Theory

## Existing Systems

In most functional languages, the type theory is based on lambda calculus. The idea is that a function accepts a single type and then returns a new type. Functions of multiple types can be represented through currying, that it accepts the first type and then returns a new function that requires one fewer types to return the final result. This makes it naturally well suited to higher order functions.

The problems occur once the type theory starts adding additional ideas. For sum types, they are handled by creating functions such as `Leaf :: v => Tree v` that create the sum types. While this works in general, it also hides a number of limits. It makes it difficult to handle method overloading and default arguments in methods. Product types can be handled by creating tuples. However, the result of this is that there exists the conversion of currying: `a -> b` vs `(a,b)`.

The other major extension is adding generics. They are typically considered as existential qualilfiers similar to `∀a. a -> a`. However, this has now introduced an entire dimension of kinds into the type system for methods.

## Catln

### Objects

Catln has a simpler view of types. As all values in Catln are a tree of named tuples, a type is simply a set of possible named tuples. For example, an object is the basic set of tuples. A class is a union of the sets of it's component objects. A parameterized type is basically just a helper for creating sets given a type. Even type properties are simply subsets of the type they are associated with. Any construct for types merely serves as ways of building different sets of tuples, forming a simple and easily manipulated paradigm.

One key difference of using sets is that the fundamental operations are different. In functional programming, the sum type and product types are the key operations for working with types. In Catln, it is really a union and intersection of the set types. While cross products are also important, they are naturally handled due to the named tuple format.

For unions, they are often done using classes. For example, here is a simple union:
```
class AlphaNumeric = Letters | Numbers
```

This says that the sets of letters and the sets of numbers combine to form the sets of AlphaNumeric. Functions from the AlphaNumeric type therefore need to handle the entire set as the domain of the function.

Intersections are done by applying multiple pieces of information to a single type. Here is an intersection using two different type properties:

```
List_len(5)_min(3)
```

#### Products and Sums

The sum and products can be easily constructed from this backbone. For a product type, the named tuples suffice. A pair could be constructed like:
```
data Pair[$A, $B]($A a, $B b)
```

A sum type could be built using the union of types with unique objects:
```
class Maybe[$T] = Just($T val) | Nothing
```

Unlike a pure sum, it is also possible to have overlapping types. Consider this alternative definition of Maybe:
```
class Maybe[$T] = $T | Nothing
```

This implementation of maybe simplifies a lot of usage by not having to write the verbose `Just` everywhere. It is also not a Sum type. In a sum type, it would be possible to represent `Maybe[Maybe[$T]]`. However, this simplified definition implies that `Maybe[Maybe[$T]] = Maybe[$T]`. I believe this is reasonable to the idea of Maybe and, in cases where you would want to differentiate `Nothing` from `Just[Nothing]`, you should really be using a custom type instead where names could be applied to describe the two forms of Nothing.

### Arrows

Along with objects, Catln also has arrows. An arrow is a rewrite rule that automatically converts one type into another based on need. It is inspired by [Scala's implicit conversions](https://docs.scala-lang.org/tour/implicit-conversions.html). The idea of an implicit conversion is that it represents a conversion that has a clear definition. Which implicit conversions should be taken can be inferred based off the methods that are called on the result of the conversion.

Which implicit conversions should be taken and when should be found during type inference. The functions called on values serve as hints for when implicits are necessary.

However, this can also result in problems if conversions happen unexpectedly or it converts to the wrong type. In theory, a well typed program should produce the same result regardless of the order of implicits as long as the implicit combination allows you to reach the desired type. This property is guaranteed through the use of [arrow testing](arrowTesting.md). Therefore, most issues that implicit conversions can cause should be found and they can generally be assumed to be safe.

This also makes handling more complex language features quite simple. Each arrow definition can correspond to many possible edges in the type graph. First, it can match multiple values for a single argument such as an integer argument. Arrows can also match classes or multiple arrows for each object within a class can be equivalent to an arrow for the entire class. Even type properties can simply be considered as nothing but arrows that apply to sets of nodes. The arrow definition can even match up to a generic. Lastly, arrows can also substitute to be applied to a subtree of the main tree at a single node in the graph. So, all fancy language features essentially convert down into the two simple representations of objects and arrows.

#### Arrow Typing

The set of values shows a lot of power when considering arrows/functions as well. Each arrow has an input and an output type, which can also be considered the domain and co-domain in set terminology.

A union of two arrows would go from the union of the domain to the union of the co-domain. This is often used for class arrows. If you define an arrow `f(Just[$T] val) -> Bool` and `f(Nothing val) -> Bool`, the union of those arrows would be the arrow `f(Maybe[$T] val) -> Bool`.

The intersection of two arrows would go from the intersection of the domain to the intersection of the co-domain. As an example, consider overlapping definitions or declarations:
```
sort(List lst) -> List
sort(List_len($x) lst) -> List_len($x)
sort(List_min($x) lst) -> List_min($x)
```

If you provide multiple definitions with the same name, they are assumed to be multiple definitions of the same function. Therefore, they should return the same result given the same input. However, the return types that are given are not necessarily precise. While it must contain all possible values, it could also contain values that are not really possible for the function to return. Because all definitions apply, everything in all the return types apply. In set terminology, the true(r) return type is the intersection of the co-domains.

In the case above, it describes how several type properties are affected by sorting the list. The main declaration of the sorting function works across the entire domain of lists. The supplementary definitions only apply when the input type to the sort function is more precise than lists in general. If you know the input list length, you can then figure out the output list length. If you don't know the input list length, then the arrow does not take effect and you won't know anything about the output list length. The same applies to the minimum element, as well as both properties in combination.

### Full Graph

The concepts of objects and arrows can be combined into a unified directed graph structure. The objects are nodes and the arrows are edges.

Each definition can be thought of as a graph in this form. This definition itself becomes a composable relationship in the graph. In addition, there are some additional assumed arrows such as going from a set to a superset as well.
