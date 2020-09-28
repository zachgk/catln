# Type System

The fundamental data structure in catln is a named tuple of the format `tupleType(argTypeA argNameA = argValA, argTypeB argNameB = argValB, ...)`. As the arguments can themselves be tuples, this forms a typed tree structure.

This format can be used to represent both code and data. For example, `addInts(Int left, Int right)` would be a tuple for the addition function. Something like `Point(Int x, Int y)` could represent a simple data type.

In addition to the data tuples, there are also arrows which convert one tuple format to another. For example, there could be an arrow `addInts(Int left, Int right) -> Int` that applies the integer addition operation. These arrows can match complicated patterns involving the data themselves, and even patterns involving the composition of multiple functions. Arrows can even take the same input tuple to multiple output tuples.

## Objects

In Catln, objects are used to define the allowed types of tuples. Each object behaves similar to a product type or a cross product. To give some examples, let me explain the two ways objects can be created: function and data types.

A function declaration looks like:
```
addInts(Int left, Int right) -> Int
```
This is an object that consists of a single product with two arguments. For now, ignore the return value.

If you overload a function, it creates two objects.
```
add(Int left, Int right) -> Int
add(Float left, Float right) -> Float
```

It is also possible to create a data type as an object. Each data declaration also creates a single object.
```
data ComplexInt(Int real, Int irrational)
```

If you noticed earlier, it is possible to have multiple objects that share the same name. The objects can be thought of like a pattern that is matched against tuples. Each object also can correspond to any number of arrows that are applied if the pattern matches. So, there is also no problem of having multiple objects match the same tuple since it just means all the arrows from all matching objects apply.

## Type Classes

Along with objects, Catln also has type classes. Each type class corresponds to the combination of one or more objects. It is essentially a sum of products. The type class can be thought of as a boolean property where a tuple has a set of type classes that it matches.

Type classes can be used to represent common properties and behaviors across multiple objects. It is analogous to enums, interfaces, and parent classes in object oriented classes. It also corresponds to both sum types and type classes in Haskell.

There are two different kinds of type classes: sealed and unsealed. A sealed type class is one where the allowed objects are fixed at creation. One example is this optional int type:
```
data OptionalInt = Some(Int val) | Nothing
```

Because they are fixed, sealed type classes can be used for pattern matching and enums.

Unsealed type classes are used to represent sums that can be extended later. This corresponds to the interface or type class.

```
class Show
instance String of Show

show(Show s) -> String
show(String s) = s
```

Here, a new class called show is declared. An instance, String, is also made of the show class.

Then, the function show is declared which should show all items of type show. Note that this syntax is a generic function declaration. The compiler will verify that all appropriate definitions exist to fulfill a claim made within all declarations. So, this syntax can also be used outside of type classes. Here, it then requires that a definition is provided for the only instance, String, which is given.

Note that it is also possible to have a class be an instance of another class. For example, all classes which are comparable also have the equality class. No special syntax is required to handle function as it merely requires that all appropriate declarations are fulfilled.

Just like Haskell, types are made instances of classes separately from their construction. For your own types, this is not important. However, this feature becomes useful when working with dependencies that are more difficult to change. You can add additional type classes to types within those dependencies, including types within the standard library.

Another idea worth mentioning is that type classes, sealed or unsealed, represent general union types. The only effect that sealing a type class has is to throw a compiler error if you try to extend one.

## Generics

It is also possible to have function, data, and classes that depend on other types. For example:
```
add<Numeric $T>($T left, $T right) -> $T

data Complex<Numeric $T>($T real, $T irrational)

data Optional<$T> = Some($T val) | Nothing
```

The type variables are written within angle braces, begin with a dollar sign and a capital letter. Here, several of them require that the type variable be instances of the Numeric type class.

They behave fairly straightforwardly in that they simply increase what the object or type is capable of matching. The type variable directly match only objects, but matching all objects within a type class is equivalent to matching the type class itself. They also can help infer outputs when the output is the result of the type variable. See [macros](macros.md) for more information.

## Type Properties

The other key design feature of the type system is type properties. Type properties are attached to types or typeclasses and add additional metadata about them. The goal of the type checker is to infer the type properties when possible and use them to better inform the choice of objects and arrows.

For example, the `List` type might have a property `List_length(Int)`. This can be used to change the return value of the `head` function depending on the inferred length of the list:

```
List<$T>.head -> Optional<$T>
List_length(Int_gt(0))<$T>.head -> $T
```

Now, calling the head function on a list will give you an optional value regardless of whether the length is known but a direct value if it is known. Therefore, you can directly use the value when it is known to be safe without writing unnecessary code to handle an impossible `Nothing` condition. When the length can't be inferred, then you do have to handle the very real `Nothing` condition. If it can be inferred, you can still treat it as an `Optional`, but it will throw a compiler warning because the `Nothing` handling code is unreachable.

For all lists, you can also assume that the length is at least 0. This leads to the relationship:
```
List -> List_length(Int_gt(0))
```

In order for this to work, it is important to describe how various arrows affect the type properties. For example:
```
List_length(Int_gt(a)).concat(List_length(Int_gt(b))) -> List_length(Int_gt(a+b))
```

Rules defined like this can enable the compiler to infer type properties even when they are not explicitly declared. Higher level functions can use these rules to automatically compute the type properties of return values.

Type properties can also be established by using conditional statements, pattern matching, and [assertions](assertions.md).

I recommend thinking about type properties in terms of sets of values. Each object and type class represent a set of possible values. Then, the type properties further restrict the set of possible values of the type into a subset of the original. This allows more information of what is known about various data types to be propagated through the program and used for both static analysis and optimization.

## Arrows

An arrow represents a conversion from one data tuple to an expression (this is the same as one data tuple to another data tuple). Each arrow consists of the Object it applies to, the resulting expression (declarations are written as arrows without an expression), a conditional guard, and possible compiler annotations. See [more](typeTheory.md).

## Other Type System Features

Following the above definitions, there are several interesting possibilities worth bringing attention to.

### Multiple Return Values

When defining a function, you can use either function overloading and/or default arguments like other languages. Unlike other languages, a function can accept the same arguments and return different return values.

For example, a `sqrt` function might return `Optional[Number]` where it can only be computed if the input is at least 0. It could also return a `Complex[Number]` including the irrational component. Either of these are reasonable return values for the function.

In Catln, these two definitions can share the same name and the desired return value will be detected during type inference. While this seems like it could have unintended effects, [arrow testing](arrowTesting.md) guarantees that it should be safe.

Furthermore, implicit conversions mean that multiple return values would be the norm rather than the exception. For example, a complex number with no irrational component can be implicitly converted to an int. The int can then be implicitly wrapped in an optional to form the same `Optional[Number]` anyway.

### Multiple level functions

The other useful type system feature is the use of multi-level function definitions. For example:
```
String concat(concat(String a, String b), String c) = concat([a, b, c])
```

Since functions are tuple trees, multi level functions can be handled naturally by simply matching against multiple levels of the tree at once.

This can be used to resolve potential performance problems like the one above. It is faster to concatenate the multiple strings all at once rather than doing them individually. Other languages have no ability to express this idea and require manual effort by the programmer. In Catln, this ensures that any way that you write your code would be optimized by the optimization rule and get the same performance.

### Partial Application

The idea of partial application is that function arguments don't have to be set at once. Just like tuples can be updated, the named tuples can be updated as well (including function calls). A partially applied tuple is one where only some of the keys have been given definitions and others are still missing them. For example:
```
baseFunctionCall = callName(arg1=1, arg2=2)
finalCall = match val
    True -> baseFunctionCall(arg3=9)
    False -> baseFunctionCall(arg3=2)
```

Once the value is used as the result type (determined through type inference), then the function will actually be called and the result returned. The partial application feature can be used to handle currying and many function manipulation problems.

One other potential usage of partial applications is to be able to call the function even when it is only partially defined as long as the missing arguments are not used. This is more common in the case of data types. For example, a circle data type given only the radius but not the center location is still sufficient to determine the circle's circumference.
