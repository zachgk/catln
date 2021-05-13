# Macros

While having strong strict typing rules can greatly reduce bugs and help guide code, you eventually run into limits. The only way to deal with these limits is to resort into short circuiting the type system using features like casting, repeating a lot of code, or you will have to start implementing macros.

I try to differentiate the terms metaprogramming and macros. Metaprogramming is used for programs or functionss that accept and modify the code from other functions before executing them. Because they often fit naturally in the existing language constructs, they don't need special handling (see [metaprogramming](metaprogramming.md)).

## Reflective Functions

Instead, macros have two types. The first type is used to refer to arrow definitions which are more unusual in terms of what they match. They may accept variable argument names or variable numbers of arguments.

Consider as an example the `zip` method. This method takes a tuple of lists into a list of tuples. A single `zip` method with fixed tuples can be created normally, but to support any kind of input tuple (or more than two input lists to zip together at once), you would need to resort to macros.

Then, the macros are implemented using reflective functions as opposed to infinite functions. So, it is not possible to create `function zip${N}` for all `N` because that would require creating infinite objects. Instead, create a single object `zip` that would apply for all numbers of things to zip.

The main thing that differentiates this from a normal function is that it may use special internal functions. For example, functions usually have fixed arguments. Some could have arbitrary arguments, but not inspect them. The most common would probably be functions to inspect or work with arbitrary number of arguments. Others could be those that look into how data structures are used or access information about the global set of objects.

Macros should be computed only during compile time. If they can change during runtime like lisp, they can be used to create infinite functions and then the only things callable within them are other macros. Restricting to compile time means that there are no performance costs and all of the same time checking and type inference will still work. It can even be used to create "pseudo-source code" which can be included in the documentation for the used versions of the macro.

I intend to use the dollar sign as the key to macros including macro types, macro values, and the special functions limited to macro context. Here is some sample pseudo code for how macros might look:

```
zip(List $args...) =
    Int size = $args.$dict.map(v -> v.size).values.min
    List(size=size, values=(i -> $args.$map(v -> v.get(i))))
```

## Code generation

In other words, the first type corresponds to a single (fancy) arrow. It can correspond to an infinite number of arrows without those expanded capabilities, but reduces that infinite number down to one. The second type of a macro lets you generate multiple arrows, but a finite number. The most common usage of this is for [defining imports](languageCompilation.md).

As an example, say we are importing a C file. Each function and type in that C file can be added as additional objects and arrows in Catln. But, as the C file is finite, it would only generate finite things into the Catln environment.
