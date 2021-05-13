# Compiler Error Statement

One of the other benefits of the powerful [type property](basics.md) system is that it is possible to identify programmer mistakes as a library developer. Then, you can provide clear error statements about what the user is doing wrong. This is part of a larger trend of making it possible to describe more compiler behavior from within the language itself.

For example, you might have a method `List.get(Int i)`. If typechecking finds that the `i` that is passed in is less than zero, there is almost certainly a mistake by the programmer. In this case, it is worth having a compiler error statement. In this instance, the compiler can give the error back to the programmer and convert the runtime error into a compile error. It might look like:

```
List.get(Int_lt(0) i) = compilerError("You attempted to access a list with an index less than zero")
List_length<Int_lt(l)>.get(Int_gt(l) i) = compilerError("You attempted to access a list with an index guaranteed to exceed the length of the list")
```

The most common use of this would be that a method is called with properties that are proven to be bad. Another case might be creating methods that allow the program to typecheck, but still throw the compilerError. This converts a vague typechecking error into a potentially more clear kind of error. One example would be assuming an optional is safe:

```
Optional<$T> -> $T = compilerError("You tried to use an optional value as if it was not optional")
```

Also under consideration, it might be worth throwing some of these as warnings instead of errors.
