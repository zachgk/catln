# Macros

While having strong strict typing rules can greatly reduce bugs and help guide code, you eventually run into limits. The only way to deal with these is to resort into short circuiting the type inference using features like casting that ignore the known types. Or, you will have to start implementing macros.

Consider as an example the `zip` method. This method takes a tuple of lists into a list of tuples. A single `zip` method with fixed tuples can be created normally, but to support any kind of input tuple (or more than two input lists to zip together at once), you would need to resort to macros.

Macros are often difficult to write when they involve "code writing" rules. This is especially true for macros that take other functions as inputs and then modify the functions. So, I tried to make clear rules to simplify the design of macros and make them fairly friendly.

First, macros should be computed only during compile time. If they can change during runtime like lisp, they can be used to create infinite functions and then the only things callable within them are other macros. Restricting to compile time means that there are no performance benefits and all of the same time checking and time inference will still work. It can even be used to create "pseudo-source code" which can be included in the documentation for the used versions of the macro.

The second is that macros should not do code rewriting or quoting. The generated code should work like regular code with the exception that it involves some additional methods and datatypes involving type reflection. The function should be evaluated such that only the macro parts of the function are actually evaluated and the rest is left as the result expression.

The most common case of the macro should be a function with unclear inputs or definitions that depend on the parameterized types. These can allow for infinite variations. You can also use macros to generate fixed numbers of source code but this will have to be finite (although it could then generate a fixed number of infinite argument methods as well).

I intend to use the dollar sign as the key to macros including macro types, macro values, and the special functions limited to macro context. Here is some sample pseudo code for how macros might look:
```
zip(List $args...) =
    Int size = $args.$map(v -> v.size).values.min
    List(size=size, values=(i -> $args.$map(v.get(i))))
```
