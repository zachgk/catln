# Choice

It is a relatively common issue in programming when you must choose between several options. While your code could be valid depending on these options, one of them must ultimately be chosen.

For example, most low-level details such as strict/lazy, parallelization, caching, memory management scheme, what data structure to use, what algorithms to use are all examples of choices. For any particular decision of **what** you want to do, all of the options for **how** you want to do it are choices.

There are two main types of choices:

- choices which are equal and valid. One example of this is choosing a sorting function. Any sorting function will return the same result given the same input
- choices which are not equal, but valid. One example of this is an approximation of something like TSP. Different approximation algorithms will return slightly different results, but they should all serve a similar purpose. As a user, it may be more important what you are approximating rather than how it is done.

In most programming languages, code must be written while specifying all of these choices. Sometimes, the options can be propagated up the call stack through arguments. However, this leaves the question of which choices to propagate and how far to propagate them. Propagating too much can result in unreadable and unmaintainable APIs that bury the essential ideas within the incidental ones. Propagating too few can result in bad performance and duplicate copies of the API that differ based on these choices such as the number of strict/lazy variations in Haskell.

A better solution is to be able to write code that defers these choices. Essentially, code is abstract over the choices. When compiling, it decides what choices to make for the final version (or versions to compile multiple variations of a function). Catln has two different mechanisms to defer these choices.

## Standard Choices

Standard choices are implemented by using simple objects and arrows. In Catln, it is possible to provide multiple definitions of the same function:

```
sort(List lst) -> List =
  #name("quicksort")
  ...

sort(List lst) -> List =
  #name("mergesort")
  ...

sort(List lst) -> List =
  #name("selectionsort")
  ...
```

When you call sort normally, it can actually use any of those definitions. This is in fact the default function calling state in Catln. Fortunately, the definitions in this case should all be equal so it requires even less concern from users of the functions. Instead of just hoping this works, it can also be verified by using [arrow testing](arrowTesting.md).

Another way to think of this is that a function signature contains both an abstract signature and a concrete definition. When you make calls, you always use the abstract signature. Then, the code is abstracted over all the possible concrete definitions.

## Variant Choices

The other case is where the results may not be equal. In this case, it is not written any differently as it doesn't change how it is executed in the language. The only change is adding the `variant` annotation to a declaration to exempt the function from arrow testing.

```
// ## Standard library
class Approx<$T>
approx(f -> $T) -> Approx<$T>
  #variant

// ## Your TSP Code

data TSPIn(...)
data TSPOut(...)

// Declaration of the function to approximate
tsp(TSPIn inputData) -> TSPOut

approx(f=tsp(TSP inputData)) -> Approx<$T=TSPResult> =
  #name("tspApproxAlgorithmA")
  ...

approx(f=tsp(TSP inputData)) -> Approx<$T=TSPResult> =
  #name("tspApproxAlgorithmB")
  ...

```

Here, it creates a standard Approximation class to represent approximations of a type `$T` and a standard approximation function that accepts a function `f` and returns the approximation of it. These will most likely be in the standard library.

Then, you declare the signature of the real tsp function along with input and output data types. This could even be a real definition too as computing the exact version might be desired in some circumstances.

Finally, you can define different implementations of the approx function applied to the tsp signature. 

Another powerful use of class choices is for data structures. For example, a definition like `List.empty` can be resolved to use a class choice to determine the appropriate type of list. This is one example of how this section will be crucial for handling [optimizations](optimization.md).

Note that with variant functions, it is also possible to apply properties to the results. For example, an approximation could have a property that provides a bound on it's accuracy. Then, you can request not just any approximation, but one with a particular minimum accuracy.

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

For a user, these name configuration files can be used to create final overrides. This follows the idea of "make it work, make it right, make it fast". The main code files are able to fully describe how to make it work and make it right. Then, only the final override needs to be touched to make it fast.

### Dispatch Choices

Another option is by providing a choice dispatch definition. This looks like a normal definition, but has no behavior besides deciding which option to choose. Here is an example of such a dispatch:

```
sort(List lst) =
  if lst.length > 256
    then sort#quickSort(lst)
    else sort#selectionSort(lst)
```

This uses the length of the list to decide between various sorting algorithms before running the algorithms themselves. This would then dispatch to the appropriate algorithm at runtime. However, it can still take advantage of type inference to remove the conditionals if it can be proven during compile time. So, if it is possible to prove the size, it will be decided during compile time instead.

It is also possible for the dispatch to rely on other traits to determine the appropriate method to dispatch to. For example, it can read compile annotations using `$hasAnnot(val=this, annot="lazy")` and `$getAnnot(val=this, annot="lazy")`. Different annotations can be used to help guide heuristics as well. In the future, I will also consider adding additional profiling strategies that can be used to determine what methods are called on the result, or possible how many methods the result can call for more powerful dispatch strategies. These will all run during compile time and be evaluated like normal [macros](macros.md).

### Automatic

Finally, the last option is to try to determine the best choice automatically. The first strategy is to follow an order of precedence. If there are multiple valid arrows but their domains differ, prefer ones with smaller (but still sufficient) domains. In other words, use the definition that is more precise to the actual input. After that, use a dispatch if one exists before looking at other functions.

The final approach, if all else fails, is to just pick an option at random. While this is far from ideal, it can always be overridden later if it proves to be a problem. The rest of the time, it is likely to work or not cause too many problems as long as the most important choices are properly accounted for. But, it means that it would still compile. For a slightly more reliable method, it can choose the last definition instead of a random one.

### Future Development

Right now, the algorithm I have described above for choice should be reasonably sufficient. It should work well in the general cases and can be improved over time with more rules given. Even in specific cases it can be manually controlled.

Other languages that lack choice essentially force there to be one option. Then, the choice is always trivial. While this solution isn't perfect, it is no doubt better.

The main problem with this choice strategy is that it is essentially greedy. It assumes that all choices are independent, but they are often not. In that case, multiple choices should be made simultaneously. It may also take advantage of other data such as profiler guided optimization, automatic Big O notation, or machine learning guided optimization. It can also make multiple choices simultaneously such as polyhedral optimization or [e-graphs](https://egraphs-good.github.io/). These are all areas for future development of the language. But, it requires the same semantics of choice as a background so using the simpler greedy algorithm should be a good start towards those future developments as well. Hopefully, they can even be added on to later versions of the language.

## Visualizing Choice

Another important task is how to visualize choice. When trying to understand what choices are made and why, having an appropriate visualization tool would make the process much easier. For low-level programming and optimizing performance choices, it would be critical.

Currently, I image a tool similar to the chrome inspector:

![Chrome Inspector](https://developer-chrome-com.imgix.net/image/BrQidfK9jaQyIHwdw91aVpkPiib2/TDNgfhI9byR4eeGQ0Xxv.png?auto=format&w=1600)

Here, the functions used at a particular level would be the HTML. For each function, it can then show all of the choices that match the function like CSS. Then, you can view all of the various choice matches and see why the final ones are chosen to figure out how to improve it.
