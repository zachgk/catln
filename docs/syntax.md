# Syntax

Here is a basic guide to the **currently implemented** syntax and features for Catln.
You can view examples of the syntax inside the [stack site](https://stack.catln.dev).
The raw files are available for both the [stack files](https://github.com/zachgk/catln/tree/master/stack) and [test files](https://github.com/zachgk/catln/tree/master/test/code).

## Comments

A comment is done using by prefixing a line with `# `. The comment includes all lines that are indented after it as well. The contents of the comment are treated as markdown so all markdown syntax (headings, bold, links, etc.) will work in the comment.

```
# This is a comment

   # Markdown title inside the comment.
```

## Declarations

To declare a value, you can write an equality:

```
# Type inferred value
x = 5

# Explicitly Typed value
Integer y = 3

```

Right now, only integers and booleans are supported. The booleans are written as `True`, `False`, or `Boolean`.

A function is written with an equals followed by the arguments:

```
double(Integer val) = val + val

# With a return type
double2(Integer val) -> Integer = val + val

# Call the function by passing in the arguments
result = double(val=5)

# Call the function while attempting to infer the argument name
result2 = double(5)

# Functions can also be defined in a method format
# It behaves like a function with the caller as an argument called "this"
Integer.double3 = this + this
result3 = 5.double3
```

Catln supports the following operators following the typical operator precedence:

- Arithmetic: `+`, `-`, `*`, `-` (unary opposite)
- Comparison: `<=` `>=`, `<`, `>`, `==`, `!=`
- Boolean: `&`, `|`, `^`, `~` (unary negation)
- Parenthesis

By convention, both values and functions should begin with a lowercase letter and use camel case.

For longer functions, they can be split over multiple lines. The earlier lines can themselves be value declarations or inner function declarations. The last line should be an expression that is interpreted as the return value.

```
sumEqProduct(Integer a, Integer b) =
    sumVal = a + b
    prodVal = a * b
    sumVal == prodVal
    
result = sumEqProduct(a=2, b=2)
```

In addition, you can also add compiler annotations to a multi-line declaration. The compiler annotations should each have their own line and can be recognized as they start with a `#` sign.

```
doublePositive(Integer val) =
    #assert(test=val>0)
    val + val
```

You can also add a conditional guard to a declaration. You can give either an if or an else guard. The else guard only activates if none of the if conditions execute.

```
abs(Integer x) if x >= 0 = x
abs(Integer x) else = x

# With return values
abs2(Integer x) if x >= 0 -> Integer = x
abs2(Integer x) else -> Integer = x
```

You can also provide an inline ternary statement.

```
abs(Integer x) = if x >= 0 then x else -1 * x
```

You can use the match statement to pattern match against a value. With the match statement, the order given does not matter. Any matching conditions can be executed for a particular value. If there is overlap among the cases, they will eventually be validated by [arrow testing](philosophy/arrowTesting.md).

```
abs(Integer x) = match x of
                  x2 if x >= 0 => x2
                  x2 else => x2 * -1
```

The case statement, unlike the match statement, executes the expressions in order. Each succeeding statement will only be tried if the preceeding ones all fail.

```
abs(Integer x) = case x of
                  x2 if x >= 0 => x2
                  x2 => x2 * -1
```


## Types

A new type object can be created through a data declaration. By convention, type objects should begin with a capital letter and use camel case.

```
data Pair(Int a, Int b)

# Create a pair
myPair = Pair(a=1, b=2)

# Pattern match against a data object in a declaration
fst(val=Pair(a, b)) = a
snd(val=Pair(a, b)) = b

# A pair with a type parameter
data Pair2<$N>($N a, $N b)
```

To create a union type, use a class declaration. Each of the elements of the union (Red, Green, and Blue here) will be treated as new type objects if they don't already exist.

```
class Stoplight = Red | Greed | Blue

# A class with a type parameter and argument
class Maybe<$T> = Just<$T>($T val) | Nothing

# A union that directly uses $T without creating a new object
# In this instance, it is not a sum type but a true union type
class Maybe2<$T> = $T | Nothing
```

You can also define annotations similarly to data objects:

```
annot #assert(Boolean test)
```

## Programs

There are two operations that Catln can execute on a program: running and building.

### Running

A runnable program is one which can be executed. It is most similar to what you would see in most programming languages. It has two signatures:

```
f -> Showable
f{IO io} -> IO
```

### Build - main

A full build produces a program that does not necessarily consist of just a single executable. The only current example is [the web test](../test/build/web.ct) which produces a single page website. In the future, other types of builds will be created.

In addition, some runnable functions can also be built. This corresponds to applying the `llvm` macro to compile the runnable into a result. The build results will be created in the `build` directory.

The possible signatures to build are:

```
f{IO io} -> IO
f -> CatlnResult
f{IO io} -> CatlnResult
```
