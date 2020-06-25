# Modules

In all languages, a module system is important to divide code into reasonably sized chunks and to ensure you are working with the desired chunks. It is also essential for the sake of naming, because otherwise names would either have to be overly long or risk conflicts. This is especially true when combining different libraries that lack knowledge of each other.

In Java for example, each file corresponds to a single class. This is only reasonable considering it's verbosity. In reality, the behaviors of a type or class are just as likely to exist in the intersection of types as focused mainly within a single type. Even once the ability to add classes separately from the type declaration, the methodology falls apart. However, the association of methods to their most relevant class is very effective from the perspective of both documentation and discoverability.

Haskell instead allows for a more free-form organization of codes. Modules can be imported either directly or qualified. However, only documentation can say which one is the right way to import for a given module. It is also difficult to discover functions without digging aimlessly through documentation. However, the organization better suites the problem domains because it is based freely on the cleanest way to view code.

## Type and Class Scoping

In Catln, functions can be attached to types and classes. Then, they are called such as `list.append(v=5)`. However, this is actually just a syntactic sugar over a direct function call `List::append(this=list, v=5)`. So, it doesn't require much additional effort at the language level besides some desugaring and special handling for docs/error messages. 

The `::` syntax is used to modularize functions within the scope of a type or class. It can also be expanded in several ways. A function can be within a scope without requiring a dot such as `List::empty`. This makes it similar to a static method. The main benefit of scoping a function is to add a context to the function that separates it from similar methods in different contexts like `Graph::empty`. It also allows for the construction of documentation pages featuring all of the methods within a type or class scope. These pages will be some of the more useful ones when programming with a library. Because of this, the code files should not necessarily be organized based on the scoping (see [documentation](documentation.md)).

Another extension of the syntax is to hold other types and classes within a scope. For example, you could create the type `List::Array` or `List::Linked` which implements the `List` class. This also helps differentiate naming for types and classes as well. For example, an `Env` type would be useful in many different contexts.

Scoping can also have multiple levels. So, values like `List::Array::empty` can be created and organized with respect to it's full scope as well as `List::Linked::empty`.

For this reason, it should also be possible to have a root scope. The base of a scope would be prefixed with a `::` such as `::List` similar to directory paths with `/`. This helps clearly differentiate an absolute vs. relative scope for a value.

## Adding to a Scope

Any value can be created with a scope by simply specifying a larger scope in the definition:
```
Optional::empty = Nothing

data List<$T>::Cons = Cons($T head, Cons<$T> tail) | Empty
```

It is also possible to add a scope to an entire code block by using an extension:
```
extend Optional
  empty = Nothing
  ...
```

All definitions, types, and classes created within the indented area would be automatically prefixed with the given scope. This is better suited to creating many definitions within the same scope.

## Calling with a Scope

The other difficulty with having a scope is having to describe which scope values and functions are in. If you always give the entire scope, it would be overly verbose. This would discourage the creation of a sufficiently large scope hierarchy to effectively organize all of the ideas.

Another option is to allow some values to be called without the scope or with only part of a scope. However, it then makes it difficult to keep track of when values require scope and when they don't. It tends to encourage overly long names to avoid the use of scopes and relatively robotic choices about importing values scopeless.

Catln proposes a new solution to the problem of scoping: type inference. Packages and files are imported by the current file making everything available within them also available in the current file as if they were all written in the same file. Even within an extend block, it should only affect the declarations and creation of new functions and types, not the usage of existing ones.

Instead, you could simply use a value such as `empty`. It would then be up to the type inference to use the context in combination with the rest of type inference to decide what the correct scoping is. Because you did not specify it with an absolute scope (`::empty`), it is registered as a relative scope. From the type system, it can then be treated as a combination of the possible scopes matching the name. However, it must be discerned down to a single scope before type inference ends otherwise it throws an "unable to infer scope" compiler error.

If such an error is thrown, the value can be replaced with one that has more scoping information. For example, that could be replaced with `List::empty` which would be inferred with the only possible resolution of `::List::empty`. The other nice part of this system is that using the type inference, it is unlikely that such scoping values would be required often. Even when they are, only the most convenient values or functions could be scoped to enable the rest to be better inferred.

## Private Definitions

Another important piece is how to make data, parameters, functions, types, and classes that end users should not be using. Not having privacy makes it difficult to maintain compatibility without a clear scope of what can be safely changed. It can makes the picture overly messy to end users by showing them things they are likely not interested in.

However, I am against strict privacy bounds. When types can be extended after creation and by different packages, it is important to be able to pierce the bounds when necessary.

Catln features a private keyword that is prefixed to various declarations to mark them private. So, there are no impacts on the naming based on privacy. However, the privacy exists only up to a scope. When declaring a method within the same scope or a subscope of the desired private value, it can be used just like any other. Outside of the scope, I currently plan to require the use of absolute scoping. So, the values would be more verbose to underscore the peculiar usage of a private value. It will also be possible to specify a specific scope following the private keyword and to have a private indent area.

```
private List::Array::empty = ...

private<List>
  ...
```
