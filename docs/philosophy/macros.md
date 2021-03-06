# Macros

While having strong strict typing rules can greatly reduce bugs and help guide code, you eventually run into limits. The only way to deal with these limits is to resort into short circuiting the type system using features like casting, repeating a lot of code, or you will have to start implementing macros.

I try to differentiate the terms metaprogramming and macros. Metaprogramming is used for programs or functionss that accept and modify the code from other functions before executing them. Because they often fit naturally in the existing language constructs, they don't need special handling (see [metaprogramming](metaprogramming.md)). Instead, macros have two types. The first type is used to refer to arrow definitions which are more unusual. They may accept variable keywords names or variable numbers of keywords.

Consider as an example the `zip` method. This method takes a tuple of lists into a list of tuples. A single `zip` method with fixed tuples can be created normally, but to support any kind of input tuple (or more than two input lists to zip together at once), you would need to resort to macros.

Macros are often difficult to write when they involve "code writing" rules. This is especially true for macros that take other functions as inputs and then modify the functions. So, I tried to make clear rules to simplify the design of macros and make them fairly friendly.

First, macros should be computed only during compile time. If they can change during runtime like lisp, they can be used to create infinite functions and then the only things callable within them are other macros. Restricting to compile time means that there are no performance costs and all of the same time checking and type inference will still work. It can even be used to create "pseudo-source code" which can be included in the documentation for the used versions of the macro.

The second is that macros should not do code rewriting or quoting. The generated code should work like regular code with the exception that it involves some additional methods and datatypes involving type reflection. The function should be evaluated such that only the macro parts of the function are actually evaluated and the rest is left as the result expression.

The most common case of the macro should be a function with unclear inputs or definitions that depend on the parameterized types. These can allow for infinite variations. 

I intend to use the dollar sign as the key to macros including macro types, macro values, and the special functions limited to macro context. Here is some sample pseudo code for how macros might look:
```
zip(List $args...) =
    Int size = $args.$map(v -> v.size).values.min
    List(size=size, values=(i -> $args.$map(v.get(i))))
```

## Type generation

In other words, the first type corresponds to a single (fancy) arrow. It can correspond to an infinite number of arrows without those expanded capabilities. The second type of a macro lets you generate multiple arrows, but a finite number. The most common usage of this is for [defining imports](languageCompilation.md).

As an example, say we are importing a C file. Each function and type in that C file can be added as additional objects and arrows in Catln.
