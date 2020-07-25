# Naming

One of the most important aspects of readable code is good naming. Some amount of naming is difficult and requires good use of the written language (e.g. English). However, the design of Catln is also intended to make naming better as well.

One of the first ways naming is improves is through the pervasive use of named tuples. While normal tuples may require slightly less code, they don't provide names to help explain what each of the arguments is. While this information might sometimes be guessed, names should be able to add further description to it. This ensures that whenever you are working with something where a name could be useful, there is an opportunity to provide one.

## Default Names

Just as important as the presence of names is the absence of them. For example consider the function `~(Bool val) -> Bool` that negates a boolean. Here, the name `val` is not presenting any information to the reader. To handle this case, I plan to add a new feature of type default names. So, you could instead write the signature `~(Bool) -> Bool` that does not features a name. Within the definition, you would use the default name attached to the `Bool` type instead. That type might be something like `bool` or `b`.

By using the default names, it is not necessary to come up with names that do not present information. By using type aliases, even usages of the same type for different purposes can also utilize grouped names. This name would then be easy to change using a code refactoring tool as all usages correspond only to the default name.

Scope naming for context

Names exclude Types

## Name Guidance

Another key piece of guidance for names is that they usually consist of a single word or idea. This is not an absolute rule, but consider if multiple words can be broken down into different pieces.

For example, instead of using an `ArrayList` type, it should use the power of [modules](modules.md) and instead be `List::Array`. Then, it is possible to refer to it by the full name if necessary or an abbreviated name of that sufficiently describes the type. There is also no need to worry about conflicts. There could be different types named `Listener` in different scopes that would be separated by the module name, instead of the object name. No need for smurf programming.

The other direction is to avoid having types inside names such as `toString`. Instead, it would be better to use the real type for linking, discoverability, and simplicity. Here, you could instead write `to<$T=String>` as the function name.
