# Choice

Choice is a fundamental problem throughout programming. However, most languages don't really acknowledge it or deal with it in any way. The general premise is simple. Every action has three parts:

1. **What** - What do you want to do? As an example, let's say we want to sort a list
2. **Options** - What are the options you can take? Here, it would be sorting algorithms like quicksort, mergesort, selection sort, bubble sort, etc.
3. **Choice** - Which choice should you make and when should you make it?

**Choice** is tricky. Maybe in general you could just choose quicksort. But, if you know a list is small then it might be better to choose selection sort. Or, if you know a list is almost sorted then it can be faster to use insertion sort.

There are many other examples of these kinds of choices. If you have a value, you can choose to compute it strictly or lazily. If you have a button on a website, you can choose what background color it should get. If you have a database, you can choose whether it should be SQL or NoSQL. If you have a scatter plot, you can choose what title to put above it.

Now, let's go into the scatter plot's title example a bit more. Imagine that your code A calls a library B which calls plot. From the perspective of the one writing the plot function, they won't have any idea what a good title is so they can only leave it blank. From the library writer's perspective, they can probably come up with a decent title but it may be formulaic or overly vague. Only from your code A will you know enough to definitely come up with a good title.

This leads to what I call the **Fundamental Rule of Choice**: the ability to make better choices depends on how well you understand the context in which the choice is made. That doesn't mean choices made with little context are necessarily bad. The library might have a formulaic title, but for many use cases it will be just fine. It is really just an acknowledgment that only with full context will you know if it is fine or not.

What separates choice from standard programming is that it crosses layers of abstraction. Normally code is supposed to obey "Separation of Concerns" or the "Single Layer of Abstraction" principle. But, obeying this would be definitively bad because it means little context and therefore poor choices. Instead, the main **What** code can follow separation of concerns while the **Choice** can combine them.

## Examples 

While it can be hard to understand choice from just a general description, let's go through a few examples.

### CSS

Probably the closest implementation to full choice is HTML and CSS. Imagine you have a function `render(Html html)`. In general, the HTML defines the **what** and the CSS defines the **choice** in how it is rendered. 

The **options** are fixed by the render function. They turn into CSS properties like `display`, `color`, and `padding`. As the rendering is part of the browser, only the browser creators are able to add or modify the CSS options.

Once again, the CSS is the **choice**. It is composed of two parts: the selectors and the properties. Each of the selectors describes a situation, when the choices will take effect. Then, the properties are the actual choices.

The selectors allow you a lot of control in how general or how fine the choice will take effect. It can be something general like `div` or specific like `div.class1 span#id2:hover > textarea`. When multiple selectors apply, the most specific one takes effect. This is similar to the rule of choice that the choice with the most knowledge (most specific) is likely to be the best.

### Low-Level Programming

Another important example of choice is low-level programming. Low-level programming revolves around the management of various aspects necessary to run a program on an actual CPU including memory management, caching computations, data structures, data types, hardware, etc.

When writing high-level code, these aspects become involved during compilation. Specifically, the compiler performs metaprogramming that transforms the high-level code into equivalent low-level code that can be directly executed. And, there are choices for how that transformation takes place. The compiler can choose to be strict or lazy, choose to use reference counting or garbage collection, can choose to use array lists or hash lists, can choose to use CPU or GPU, etc.

This behaves like the fundamental rule of choice. The general low-level metaprogramming strategy should provide a good default, but it is ultimately limited. This is the source of the performance gap between low-level and high-level programming.

However, low-level languages resolve the gap by making programming worse. Rather than focusing on the main ideas you want to express, you also have to mix in ideas related to the low-level nature of the code in violation of separation of concern. Choice enables you to leave the main code readable and not have to specify low-level details on non-critical parts of the code. When you desire, you can still override any choices to better optimize the program.

It also works well for the adage "make it, work make it right, make it fast". The "make it work" would be creating basic definitions while neglecting all choices. Then, "make it right" would be adding tests after those definitions. Finally, "make it fast" would be adding choices through apply statements to those definitions. Best of all, each of these steps can be done with completely separate code and there is no risk that "making it fast" could harm the readability or introduce new bugs.

## Implementation

With a few examples, now we can look into how choice actually looks like. There are two main ways that choice manifests: choice of function and choice of arguments.

### Choice of Function

Choice of function results from the use of overlapping function definitions in Catln. This means that functions can have the same input types and function signature but different definitions. Every use of an overlapping function creates a choice of which function to use. Here is an example of creating several overlapping sort functions:

```
List.sort -> List
  # This creates a declaration of sort which all definitions should follow.
    While not strictly required, it is a good practice.

# Next, we will provide a definition of sorting which we will call "quick" for quicksort.

List.sort -> List =
  #name("quick")
  ...


# We gave quicksort a name annotation to differentiate them.
  All sort functions share the same function name because they represent the same "what".
  So, a secondary name is required to refer to one particular algorithm vs another.
  
# As this practice is quite common, the next two use a syntactic sugar for the name annotation

List.sort"merge" -> List =
  ...

List.sort"selection" -> List =
  ...
```

So, when you call a function by `lst.sort`, it is really a generic or abstract call. It could refer to any of the sort definitions and which particular one is left to choice.

A particular variation can be called using `lst.sort#name("quick")` or `lst.sort"quick"`.

Finally, you can use an apply statement like:

```
apply sort"quick" sort"selection" sort
  #name("selection")
```

The apply statement allows you to modify the sort of an existing function. This is the preferred way to write it because it keeps the main function cleaner. And, this way can work no matter how far down sort is in the call stack from you.

Another useful option is dispatch or heuristic implementations. These implementations can provide a basic heuristic to make decisions among the function choices. As an example, here is a heuristic that might be made for choosing a sorting algorithm based on the length of the list:

```
List.sort = if this.length > 256 then this.sort"quick" else this.sort"selection"
```

They are useful for providing low-context choices or default behaviors that attempt to leverage the type system to gain a bit of extra understanding about the context. Here, the length filter could be resolved automatically depending on the possible list lengths the type system determines. Another factor that could be added to the heuristic is whether the data type in the list is small and should be sorted directly (like ints) or large and should be sorted with pointers to the data (like strings).

### Choice of Arguments

Along with the choice of functions is the choice of arguments. These work similarly to optional arguments in other programming languages. The main difference is that optional arguments are passed in only by the calling function, but choice arguments can be passed in from any part of the code regardless of how many function calls are between them.

As an example, let's say we have the scatter plot function with a title as a choice argument. It would look like:

```
plot(List[Num] x, List[Num] y, Optional[String] #title Nothing) -> Image = ...
```

In this example. we have the `plot` function with two normal arguments `x` and `y` and a single choice argument `#title`. Title is a choice argument because it's name begins with `#` and it has the default value `Nothing`.

Plot can be called by adding the title annotation to the call of plot such as `plot#title("Title")(x, y)` or `plot(x=x, y=y, #title="Title")`.

It can also use an apply statement like:

```
apply foo plot
  #title("Foo plot")
```

## Quantifying Choice Outputs

One thing to be aware of with choice is that arrows in Catln are not equivalent to functions, either in math or other programming languages. Functions map each input to a single output, but arrows can map an input to many different outputs. One example is the plotting we saw earlier. Specifically, the arrows are equivalent to a mathematical statement and can be read as "∀ input arguments, ∃ output". 

However, there is a special class of arrows that are uniquely quantified. They are equivalent to the stronger statement that "∀ input arguments, ∃! output". And, these are equivalent to functions. Here is how a uniquely quantified arrow can be indicated with one example, sorting:

```
List.sort -> List
  #unique
```

With sorting, we do know that regardless of the sorting algorithm used, the sorted list will always be the same. The major benefit of uniquely quantified arrows is that it is possible to use [arrow testing](arrowTesting.md) with them so it is best to indicate them when possible.

When working with non-unique arrows, it is important to be aware of different possible outputs. The general assumption is that the arrow represents a possibly vague intent and anything that successfully fulfills that intent would be valid.

One good way to think about this is like a journey. The arrow has a starting point and one or more ending points. Choice allows you to control everything between the start and the end.

To control what output is returned, there are two ways. The first is to use choice directly such as the earlier example of a title when plotting.

The second way is to use types to filter the endpoint. As an example, take computing with approximate numbers. Because working with perfectly precise numbers is difficult and often expensive, most of the time you should work with approximate numbers like floating point numbers instead.

If you had an approximate number `n` and used it in `printf("%.2f", n)`, it prints `n` with up to 2 decimal places. This means that you only need `n` to be accurate to 2 decimal places. In this case, it is possible to compute `n` faster using faster data structures (float instead of double), less accurate approximations, and more numerical instability. On the other side, requiring more decimal places would mean slower but more accurate computations.

The way this works is that it reduces the possible output types of the arrow producing `n`. Now, the choices have to be made in order to produce the desired output type, not just any possible choices. The same thing can be used for various other restrictions on the arrow up to the capabilities of the type system. Even if you don't use something that automatically adds restrictions like `printf`, a type can be manually given to `n` to help determine what the requirements for it are.

## Visualizing Choice

Another important task is how to visualize choice. When trying to understand what choices are made and why, having an appropriate visualization tool would make the process much easier. For low-level programming and optimizing performance choices, it would be critical.

Currently, I image a tool similar to the [chrome inspector](https://developer.chrome.com/docs/devtools/css/reference/).

The inspector lets you visualize the equivalent of the function execution for a particular function call (with inputs). You can expand different levels of the tree to see what sub-calls were made in each function.

On the right, you can see two panes. The left of the two panes shows all of the "choice selectors" that are being applied in order of precedence. This lets you see the specificity, how they are overriding the choice, and which choices are being made.

Finally, the rightmost pane shows all of the final choices which are made for the particular call you are looking at. The one modification is that this section would likely to be heavier on documentation and links to documentation. Unlike with CSS where the options are fixed, the options would vary heavily and figuring out what choices are available will likely be an important task.

## Design

So far, I have mostly focused on a micro view of choice that is focused on individual choices made. It is also useful to think about choice from a macro view of how all of the choices form a whole which is larger than the sum of it's parts.

Choice, like many fundamental problems, are recognized by many people. They will separately try to solve it or part of it and often give it different names and terminologies. When I think about the macro view of choice, I tend to use one of those synonyms: design. As a synonym, it also contains the same three components:

1. **What = Design Requirements** - What does it take to be a successful design?
2. **Options = Design Choices** - What are the design choices that can be made? Given the design choices, it is possible to expand them out to form the full set of possible designs which is called the **Design Space**
3. **Choice = Design** - The actual design work of making design choices.

When looking at the whole of design, we first need to establish the purpose of design. Really, let's start with the purpose of not-design: the **What**, **Design Requirements**, or modeling component. The goal of the model is only to establish the truth of the problem domain. It should be organized matching the natural organization of the problem, include nuances of the domain, and attempt to promote pure understanding.

Then, the purpose of design is to best accomplish various goals for the code. As an example, let's say we are designing a web service. One of the goals is that the service should have low latency. What makes design especially tricky is that there are often multiple goals and those goals are often contradictory. In our web service, another goal might be to have the service be low cost. But, low cost makes it difficult to still maintain low latencies so a tradeoff will have to be made between these goals.

In fact, it is often worse. Another goal might be to have the service be robust and highly available and that is contradictory with both latency and cost. But, even something like latency is not that simple as there could be many pages and actions in the web service that each have their own latencies and choices like the database can have wide-reaching ramifications on all of them.

Another example of design is the appearance of a website. There are a number of design goals to this such as looking nice, following conventions from other websites, not being too similar to other websites so it doesn't feel like copying, working well for disabilities and color-blindness, have information density for power users, have sparser information so it feels cleaner and doesn't confuse newer or elderly users, and all of the performance design goals. With these design goals, many of them are even subjective.

As a final example, consider performance optimization. The three largest contradictory design goals are fast runtime, low memory usage, and low binary size. But, it often depends on the particular input given to a program as different inputs would have different performance characteristics. It is not possible to have something be optimal for all inputs. Even if you make many variations of the code and functions in it to work on different sets of inputs, it will still result in a large binary size.

So, every design has goals and often contradictory ones. This is why humans are still important to these processes to look at the design choices, the tradeoffs that are made, and determine whether it is worth it.

## Automatic Design

Given this, a legitimate question is whether it is possible to automate some or all of the design process. It is and I think this is a valuable addition to the software development field. There are two requirements to do this successfully:

First, there must be a way to automatically evaluate designs. While it may be theoretically possible if designs can be compared, it is best if a numeric score could be given to designs and the goal was to find a design to minimize/maximize that score. This means that weights would have to be given to the various tradeoffs to combine them into a single score.

Second, you must establish which choices to automate. If you are trying to improve the performance of a website, you wouldn't want the appearance of the site to be changed as well. So, you need to establish which choices are subject to optimization and which ones will be fixed.

Now, let's go through a few examples of automatic design. One of the more useful is probably a compiler strategy known as Profile-guided Optimization (PGO). One reason compiling is difficult is that it must compile without knowledge of typical inputs. PGO gives the compiler some number of sample inputs, profiles them, and uses the profile to improve the performance on those inputs.

This can be treated as an automatic design problem. The evaluation would be the performance (runtime and memory usage). The choices to optimize would be the low-level and compiler choices. As these would be the same for most usages of PGO, it could be integrated into the compiler for easier usage.

A more creative example would be to use automatic design to make a nice looking website. This case is interesting as evaluating a website is subjective so it can't be simply measured. Instead, it could evaluate the designs using machine learning. Then, the choices would be based on appearance such as colors, sizes, spacing, shadows, etc.

A final example would be that machine learning itself can be treated as an automatic design problem. The training would be the automatic design and production would be using the design. In this case, both the model structure and the model parameters would be treated as choices while the loss would be the automatic design goal.

When doing automatic design, efficiency is a large problem. Even when viewing just the restricted design spaces you are trying to optimize, it is usually too large to test each element in the space. So, it must test a random subset of designs to find the best one. Different strategies can be used and tested to determine this subset such as evolutionary methods, Bayesian optimization, or gradient descent.

Finally, once a design is chosen it must be serialized into some format. The simplest format is to serialize into apply rules similarly to users will write and then it can be read as standard Catln code. Other imported files could also be used for large choices such as deep learning model parameters. And, the serialization would also be useful for design choices that require persistence such as website appearance or database used that shouldn't change by accident.

## Future Development

Right now, the algorithm I have described above for choice should be reasonably sufficient. It should work well in the general cases and can be improved over time with more rules given. Even in specific cases it can be manually controlled.

Other languages that lack choice essentially force there to be one option. Then, the choice is always trivial. While this solution for choice isn't perfect, it is no doubt better.

The main problem with this choice strategy is that it is essentially greedy. It assumes that all choices are independent, but they are often not. In that case, multiple choices should be made simultaneously. It can use some strategies such as adding choice costs instead of absolute choices, polyhedral optimization, or [e-graphs](https://egraphs-good.github.io/). The only difficulty is that it also makes manual control more complicated.

These are all areas for future development of the language. But, it requires the same semantics of choice as a background so using the simpler greedy algorithm should be a good start towards those future developments as well. Hopefully, they can even be added on to later versions of the language.
