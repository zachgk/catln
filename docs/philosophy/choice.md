# Choice

It is a relatively common issue in programming when you must choose between several options. While your code could be valid depending on these options, one of them must ultimately be chosen.

There are two main types of choices:
- choices which are equal and valid. One example of this is choosing a sorting function. Any sorting function will return the same result given the same input
- choices which are not equal, but valid. One example of this is an approximation of something like TSP. Different approximation algorithms will return slightly different results, but they should all serve a similar purpose. As a user, it may be more important what you are approximating rather than how it is done.

In most programming languages, code must be written while specifying all of these choices. Sometimes, the options can be propagated up the call stack through arguments. However, this leaves the question of which choices to propagate and how far to propagate them. Propagating too much can result in unreadable and unmaintainable APIs that bury the essential ideas within the incidental ones. Propagating too few can result in bad performance and duplicate copies of the API that differ based on these choices such as the number of strict/lazy variations in Haskell.

A better solution is to be able to write code that defers these choices. Essentially, code is abstract over the choices. Catln has two different mechanisms to defer these choices.

## Object Choices

The first is by using simple objects and arrows. In Catln, it is possible to provide multiple definitions of the same function:

```
sort(List lst) -> List = ... // quicksort
sort(List lst) -> List = ... // merge sort
sort(List lst) -> List = ... // selection sort
...
```

When you call sort normally, it can actually use any of those definitions. This is in fact the default function calling state in Catln. Fortunately, the definitions in this case should all be equal so it requires even less concern from users of the functions. Instead of just relying on this, it can also be verified by using [arrow testing](arrowTesting.md).

## Class Choices

For cases where choices may not be equal, a class is used instead of an object. Consider this example:

```
class Approx<$T>
data TSP(...)

tspApproxA(TSP inputData) = ...
instance tspApproxA of Approx<$T=TSP>
Approx(TSP val) = tspApproxA(inputData=val)

tspApproxB(TSP inputData) = ...
instance tspApproxB of Approx<$T=TSP>
Approx(TSP val) = tspApproxB(inputData=val)
```

Here, it creates a standard Approximation class (this definition would likely be part of the standard library). Then, it has a standard form for TSP input data. You can think of this as a graph.

To create an instance, three steps are required. First, create the actual function to describe the particular approximation strategy. This strategy would then be a member of the Approx class. Lastly, provide the definition for how to convert from the general class form of `Approx` to the particular choice form of `tspApproxX` in the form of a simple definition.

In Catln, a class name can not be reused as an object name. Therefore, any definitions that use a declared class name are known to be a class choice. They will not be subject to arrow testing, but will otherwise be treated fairly normally.

Another powerful use of class choices is for data structures. For example, a definition like `List.empty` can be resolved to use a class choice to determine the appropriate type of list. This is one example of how this section will be crucial for handling [optimizations](optimization.md).

## Resolving Choices

Given this power, the next question is how to make the choices.

### Choice by Name

The first option is by using the `#name(String name)` compiler annotation. The name can be applied to both a definition of a function and a call of the function. This can be used to manually specify the value of the choice.

While this seems simple, it is actually quite powerful. If necessary, annotations can be overwritten by using an apply statement:

```
apply sort#name("quickSort") sort#name("selectionSort") sort
  #name("selectionSort")
```

The apply statement can also describe a path of definitions to describe where to apply the annotations. Files that focus on these apply statements can be treated like configuration files where additional information to resolve these choices are. These statements work where the latest one takes effect, so they can be part of both libraries and directly used by users.

For a user, these name configuration files can be used to create final overrides. This follows the idea of "make it work, make it right, make it fast". The main code files are able to fully describe how to make it work right. Then, only the final override needs to be touched to make it fast.

### Dispatch Choices

Another option is by providing a choice dispatch definition. This looks like a normal definition, but has no behavior besides deciding which option to choose. Here is an example of such a dispatch:

```
sort(List lst) =
  if lst.length > 256
    then sort#quickSort(lst)
    else sort#selectionSort(lst)
```

This uses the length of the list to decide between various sorting algorithms before running the algorithms themselves. This would then dispatch to the appropriate algorithm at runtime. However, it can still take advantage of type inference to remove the conditionals if it can be proven during compile time. So, if it is possible to prove the size, it will be decided during compile time instead.

It is also possible for the dispatch to rely on other traits to determine the appropriate method to dispatch to. For example, it can read compile annotations using `$hasAnnot(val=this, annot="lazy")` and `$getAnnot(val=this, annot"lazy")`. Different annotations can be used to help guide heuristics as well. In the future, I will also consider adding additional profiling strategies that can be used to determine what methods are called on the result, or possible how many methods the result can call. These will all run during compile time and be evaluated like normal [macros](macros.md).

### Automatic

Finally, the last option is to try to determine the best choice automatically. The first strategy is to follow an order of precedence. If there are multiple valid arrows but their domains differ, prefer ones with smaller (but still sufficient) domains. In other words, use the definition that is more precise to the actual input. After that, use a dispatch if one exists before looking at other functions.

The final approach, if all else fails, is to just pick an option at random. While this is far from ideal, it can always be overridden later if it proves to be a problem. The rest of the time, it is likely to work or not cause too many problems as long as the most important choices are properly accounted for.

In the future, more sophisticated techniques can be used to make these automatic choices. They can rely on things like profiling all of the tests and examples to explore how functions are actually used. It can try to prove Big O values of functions.
