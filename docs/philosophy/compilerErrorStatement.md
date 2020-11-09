# Compiler Error Statement

One of the other benefits of the powerful [type property](basics.md) system is that it is possible to identify programmer mistakes as a library developer. Then, you can provide clear error statements about what the user is doing wrong. This is part of a larger trend of making it possible to describe more compiler behavior from within the language itself.

For example, you might have a method `List.get(Int i)`. If typechecking finds that the `i` that is passed in is less than zero, there is almost certainly a mistake by the programmer. In this case, it is worth having a compiler error statement. In this instance, the compiler can give the error back to the programmer and convert the runtime error into a compile error.

The most common use of this would be that a method called with properties that are proven to be bad. Another case might be creating methods that allow the program to typecheck, but still throw the compilerError. This converts a vague typechecking error into a potentially more clear kind of error. I don't have any examples for this off hand, but it would be worth considering in the future.

Also under consideration, it might be worth throwing some of these as warnings instead of errors. Likewise, the usefulness of these warnings might become more clear in the future.
