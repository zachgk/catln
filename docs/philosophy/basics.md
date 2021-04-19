# Basics

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

If you overload a function, it creates two objects that share the same name, but with different arguments.
```
add(Int left, Int right) -> Int
add(Float left, Float right) -> Float
```

It is also possible to create a data type as an object. Each data declaration also creates a single object.
```
data ComplexInt(Int real, Int irrational)
```

## Type Classes

Along with objects, Catln also has type classes. Each type class corresponds to the combination of one or more objects. If each object is a product, the type class is a union of products. The type class can also be thought of as a boolean property where a tuple has a set of type classes that it matches.

Type classes can be used to represent common properties and behaviors across multiple objects. It is analogous to enums, interfaces, and parent classes in object oriented classes. It also corresponds to both sum types and type classes in Haskell.

There are two different kinds of type classes: sealed and unsealed. A sealed type class is one where the allowed objects are fixed at creation. One example is this optional int type:
```
data OptionalInt = Some(Int val) | Nothing
```

Because they are fixed, sealed type classes can be used for pattern matching and enums.

Unsealed type classes are used to represent unions that can be extended later. This corresponds to the interface or type class.

```
class Show
show(Show s) -> String

every String isa Show
show(String s) = s

every Boolean isa Show
show(True) = "True"
show(False) = "False"
```

Here, a new class called show is declared.

Then, the function `show` is declared which should show all items of type show. Note that this syntax is a generic function declaration. The compiler will verify that all appropriate definitions exist to fulfill a claim made within the declaration. So, this syntax can also be used outside of type classes.

Next, two instances of the `Show` class are added: String and Boolean. Each must also fulfill the `show` definition as they have been added to the domain of the function. They are described using an `every _ isa _` statement.

It is also possible to have a class extend another class. For example, all classes which are comparable also have the equality class. No special syntax is required to handle function as it merely requires that all appropriate declarations are fulfilled.

Just like Haskell, types are added to classes separately from their construction. For your own types, this is not important. However, this feature becomes useful when working with dependencies that are more difficult to change. You can add additional type classes to types within those dependencies, including types within the standard library.

Another idea worth mentioning is that type classes, sealed or unsealed, represent general union types. The only effect that sealing a type class has is to throw a compiler error if you try to extend one. The implementation, otherwise, is completely identical.

## Generics

It is also possible to have function, data, and classes that depend on other types. For example:
```
add<Numeric $T>($T left, $T right) -> $T

data Complex<Numeric $T>($T real, $T irrational)

class Optional<$T> = Some($T val) | Nothing
```

The type variables are written within angle braces, begin with a dollar sign and a capital letter. Here, several of them require that the type variable be instances of the Numeric type class.

They behave fairly straightforwardly in that they simply increase what the object or type is capable of matching. The type variable directly match only objects, but matching all objects within a type class is equivalent to matching the type class itself. They also can help infer outputs when the output is the result of the type variable. See [macros](macros.md) for more information.

## Type Properties

The other key design feature of the type system is type properties. Type properties are attached to types or typeclasses and add additional metadata about them. The goal of the type checker is to infer the type properties when possible and use them to better inform the choice of objects and arrows.

For example, the `List` type might have a property `List_length<Int>`. This can be used to change the return value of the `head` function depending on the inferred length of the list:

```
List<$T>.head -> Optional<$T>
List_length<Int_gt(0)><$T>.head -> $T
```

Now, calling the head function on a list can give you either an optional or direct value. Therefore, you can directly use the value when it is known to be safe without writing unnecessary code to handle an impossible `Nothing` condition. When the length can't be inferred, then you do have to handle the very real `Nothing` condition. If it can be inferred, you can still treat it as an `Optional`, but it will throw a compiler warning because the `Nothing` handling code is unreachable.

For all lists, you can also assume that the length is at least 0. This leads to the relationship:
```
List -> List_length<Int_gt(0)>
```

In addition, it is important to describe how various functions and arrows affect the type properties. For example:
```
List_length<Int_gt(a)>.concat(List_length<Int_gt(b)>) -> List_length<Int_gt(a+b)>
```

Rules defined like this can enable the compiler to infer type properties even when they are not explicitly declared. Higher level functions can use these rules to automatically compute the type properties of return values.

To clarify, type properties can accept an input either as a type or an expression. Types, like usual, are provided using `<>` while expressions use `()`. When they are compared, it checks using the type of the expression and the direct types.

I recommend thinking about type properties in terms of sets of values. Each object and type class represent a set of possible values. Then, the type properties further restrict the set of possible values of the type into a subset of the original. This allows more information of what is known about various data types to be propagated through the program and used for both static analysis and optimization.

Type properties exist in a somewhat similar space to [refinement types](https://en.wikipedia.org/wiki/Refinement_type). The key difference is that they are defined entirely within Catln itself while most refinement type systems use an external tool such as [Z3](https://en.wikipedia.org/wiki/Z3_Theorem_Prover). That means that the external tool would be limited to operating on preset domains of knowledge while type properties can grow as more things are implemented in the language.

Another difference is that it is also possible to use type properties for more abstract or computed values. You could add a `String_hasPassword<Boolean>` property to help check whether it is possible that a password is contained in a String. This could be used to sanitize the results before saving in a database or sending to a browser. Another possible property would be something like `HTML_classList<List<$T=String>>`. This property can be used to compute the possible classes an HTML element could have, useful for pruning the CSS file.

## Arrows

An arrow represents a conversion from one data tuple to an expression (this is the same as one data tuple to another data tuple). Each arrow consists of the Object it applies to, the resulting expression (declarations are written as arrows without an expression), a conditional guard, and possible compiler annotations. See [more](typeTheory.md).

The objects can be thought of like a pattern that is matched against tuples. Each object then corresponds to zero or more arrows that are applied if the pattern matches (zero for pure data type). So, there is also no problem of having multiple objects match the same tuple since it just means all the arrows from all matching objects apply.

Arrows are created by providing either a declaration or definition. A declaration features the object (or pattern) on the left side and the resulting type on the right side. It states that there should exist a definition for this type, but doesn't explicitly state what it is.

```
show(Show s) -> String
```

A definition, unlike a declaration, does return a value. The expression on the right side of the `=` is the tuple that is returned as a result of the arrow. It can involve sub-definitions and sub-declarations as well as arguments from the pattern matching on the left hand side of the `=`.

```
not(val=True) = False
not(val=False) = True
```

Both a declaration and definition create two components: an object and an arrow.

## True Statements

The other major syntactic structure for effective type properties is an implication. The purpose of the implication is to support true statements. For example, consider this simple absolute value function:

```
abs(Int i) = if i >= 0
               then i
               else -i
```

This definition has a conditional "if" statement that branches the code based on the definition of `i`. However branching the code is also the process of learning about `i`. While `i` can be any integer in the function at large, we know it is limited to `Int_gte(0)` within the then branch. The reason we know this is because within the branch we can further assume the statement `i >= 0` is true. Likewise, we know within the else branch that `i` is of type `Int_lt(0)` because `not(i >= 0)` is true.

To convert between expressions and the type properties that they imply are implication statements. Here are some examples:

```
# the left part of a true boolean expression is also a true statement
operator&(Boolean l) .: l

# the right part of a true boolean expression is also a true statement
operator&(Boolean r) .: r

# the left side of a >= has type Int_gte(r)
operator>=(Int l, Int r) .: l :: Int_gte(r)

# the right side of a >= has type Int_lt(l)
operator>=(Int l, Int r) .: r :: Int_lt(l)
```

These implications are used to adjust the uses of a type. The support for conjunction of true statements works like defined in the example above. A disjunction can still be represented by combining the information into the object containing several types. For a function definition, this is typically the calling function would contain the disjunction information about it's arguments.

## Annotation Applications

The final major structure is an annotation application. These applications are used to provide additional annotations after a function is written.

It can also be used to apply to only certain kinds of calls to the function. It is designed similarly to CSS selectors and many of the same capabilities are available. Instead of using a DOM tree, it matches against the function call tree. Here is an example:

```
apply sort#name("quickSort") > sort#name("selectionSort") sort
  #name("selectionSort")
```

The main use of the applications is to provide more information towards [choice](choice.md). It can also be moved into a separate file to work like a configuration. Note that while annotations can give instructions to tools such as the compiler, testing, or webdocs, it doesn't change the fundamental knowledge environment.

## Other Type System Features

Following the above definitions, there are several interesting possibilities worth bringing attention to. These are not additional language features, but cases within the above features that are worthy of attention.

### Multiple Return Values

When defining a function, you can use either function overloading and/or default arguments like other languages. Unlike them, a function can accept the same arguments and return different return values.

For example, a `sqrt` function might return `Optional<Number>` where it can only be computed if the input is at least 0. It could also return a `Complex<Number>` including the irrational component. Either of these are reasonable return values for the function.

In Catln, these two definitions can share the same name and the desired return value will be detected during type inference. While this seems like it could have unintended effects, [arrow testing](arrowTesting.md) guarantees that it should be safe.

Furthermore, implicit conversions mean that multiple return values would be the norm rather than the exception. For example, a complex number with no irrational component can be implicitly converted to an int. The int can then be implicitly wrapped in an optional to form the same `Optional<Number>` anyway.

### Multiple level functions

Another useful type system feature is the use of multi-level function definitions. For example:
```
String concat(concat(String a, String b), String c) = concat([a, b, c])
```

Since functions are tuple trees, multi level functions can be handled naturally by simply matching against multiple levels of the tree at once.

This can be used to resolve potential performance problems like the one above. It is faster to concatenate the multiple strings all at once rather than doing them individually. Most languages have no ability to express this idea and require manual effort by the programmer. In Catln, this ensures that any way that you write your code would be optimized by the optimization rule and get the same performance.

This feature is also very useful as the backbone of [metaprogramming](metaprogramming.md).

### Partial Application

The idea of partial application is that function arguments don't have to be set at once. Just like tuples can be updated, the named tuples can be updated as well (including function calls). A partially applied tuple is one where only some of the keys have been given definitions and others are still missing them. For example:
```
baseFunctionCall = callName(arg1=1, arg2=2)
finalCall = match val
    True -> baseFunctionCall(arg3=9)
    False -> baseFunctionCall(arg3=2)
```

Once the value is used as the result type (determined through type inference), then the function will actually be called and the result returned. The partial application feature can be used to handle currying and function manipulation.

One other potential usage of partial applications is to be able to call the function even when it is only partially defined as long as the missing arguments are not used. This is more common in the case of data types. For example, a circle data type given only the radius but not the center location is still sufficient to determine the circle's circumference.
