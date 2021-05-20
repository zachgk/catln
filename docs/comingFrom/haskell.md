# Coming From Haskell

This is a short guide of things to keep in mind when using Catln for programmers from a Haskell background.

## Arrow notation

Both Haskell and Catln use an arrow notation `a -> b` in types. However, they have different meanings.

In Haskell, `a -> b` would be read that you have a function which accepts an argument of type `a` and returns a value of type `b`.

If you see `a -> b` in Catln, it means that something of type `a` can be converted into something of type `b`. For example, `List -> Set` says that any list can be converted into a set.

To describe a function type, it would look like `f(a) -> b` where `f` is the function name or the argument name of the function. One benefit of this system is that having a name for `f` makes it easy to describe the function. It is also possible to have classes of functions such as `Monotonic f(a) -> b` where the class `Monotonic` is applied to `f`.
