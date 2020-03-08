# Type System

The type system consists of two components: types and typeclasses. A type is a set of data put together with common properties and behaviors. It is analogous to classes in object oriented languages.

The second component is a typeclass. The typeclass is a higher level or more generic set of behaviors and proeprties that other classes can then implement. In object oriented languages, it is analogous to interfaces or parent classes.

The best way to think about typeclasses are as a boolean property. Each type can then either extend the typeclass or not leading to a set of extended typeclasses.

As part of this, there can also be dependent typeclasses. For example, a Comparable typeclass depends on an Equality tyepclass. Anything with the comparable behavior would exhibit all the behaviors of an equality and definitions are shared between the two.

There are two notable differences between typeclasses and OO classes. First is that they naturally support "multiple inheritance". This is a key feature in order to implement interfaces but the potential issues are resolved using [arrowTesting](arrowTesting.md).

The second key difference is that types are declared instances of a typeclass after the initial definition, not during. For your own types, this is not a big deal. This feature becomes important when dealing with dependencies. You are able to add additional typeclasses to types that are in those dependencies or even in the standard library.

## Type Properties

The other key design feature of the type system is type properties. Type properties are attached to types or typeclasses and add additional metadata about them. For example, the `List` might have a property `List_Length(Int_GT(0))` which can be assumed for all lists. These properties can be declared as requirements for functions. It can also be used to determine function output types such as `List.head`. The result would be an `Optional[Int]`, but the head of a list with length at least one would instead be an actual `Int`. 

In order for this to work, the type properties also have to be inferrable. For example:
```
List_Length(Int_GT(a)).concat(List_Length(Int_GT(b))) -> List_Length(Int_GT(a+b))
```

Rules defined like this can enable the compiler to infer type properties even when they are not explicitly declared. Even functions that call other functions with clear property inference rules can then have type inference capabilities by proxy.


## Other Type System Features

There are other incidental features of the type system that also offer benefits to users.

### Multiple Return Values

When defining a function, you can use either function overloading and/or default arguments like other languages. Unlike other languages, a function can accept the same arguments and return different return values.

For example, a `sqrt` function might return `Optional[Number]` where it can only be computed if the input is at least 0. It could also return a `Complex[Number]` including the irrational component. Either of these are reasonable return values for the function. In this language, these two definitions can share the same name and the desired return value will be detected during type inference. While this seems like it could have unintended effects, [arrow testing](arrowTesting.md) guarantees that it should be safe.

Furthermore, implicit conversions mean that multiple return values would be the norm rather than the exception. For example, a complex nuber with no irrational component can be implicitly converted to an int. The int can then be automatically wrapped in an optional to form the same `Optional[Number]` anyway.

### Multiple level functions

The other useful type system feature is the use of multi-level function definitions. For example:
```
String concat(concat(String a, String b), String c) = concat([a, b, c])
```

Since functions are esentially tuples, multi level functions can be handled naturally by an implicit to allow defining functions with their inputs as functions. This can be used to resolve potential performance problems with bad impliciations such as the string concatenation above. It is more performant to concatenate the multiple strings all at once. However, with the rule above, any way that you write your code would be able to follow the optimization rule and get the same performance.
