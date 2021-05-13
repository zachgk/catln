# Metaprogramming

Metaprogramming is writing code that treats other code as it's data. While problems that require metaprogramming are not that common, lacking strong capabilities when they are needed can make it very difficult. There are also many problems that can take advantage of metaprogramming to be simpler and more applicable.

To motivate this discussion, let me give a few examples of metaprogramming problems:

- Simple optimization rules such as `++(String l, r=++(String l=rl, String r=rr)) = concat([l, rl, rr])`. These rule should apply to all code that is written and can help resolve what would otherwise be performance problems. These can help users focus efforts entirely on readability without needing to also worry about performance, but can be especially critical when working across function boundaries.
- Auto-differentiation. This takes a function with numerical input(s) and a numerical output and figures out the derivative of the input(s) with respect to the output. For deep learning, this is an important task because finding the effect of various model parameters on the output accuracy is the main input to model optimization. It requires applying differentiation to all of the code used in the model to figure out how each called function works and combining them all to find the overall differentiation.
- AutoCloud. One of the goals in Catln is to create a metaprogram function `autoCloud(webApplication)` that will take a web application designed for a single server and produce an output designed to work across a cloud provider such as AWS. It would need to synchronize database accesses and local state and make use of various services such as lambda/beanstalk for computation or cloudfront for serving static content. It can also include best practices such as metrics, logging, deployment, and migrations.
- Compilation. In Catln, the fundamental compilation task of moving from high-level Catln code to the low-level executable (llvm but could also be raw assembly) is done using metaprogramming. In fact, the main is defined as just `main = llvm(c=mainx)`. First, this helps move work from the compiler itself to the standard library to make it easier to create new versions and improve. It also makes the low-level features such as strict/lazy or memory management strategies adjustable using choice. New compilation areas such as specialized code for compiling highly parallelizable N-dimensional data can be added by just importing an additional library. Finally, it helps bridge the gap between high-level and low-level programming by ensuring code can live anywhere in between.

The goal of metaprogramming is mostly the same as programming. It should be easy to write and easy to read. The type system should help prevent bugs and mistakes. There needs to be testing, examples, and documentation. In my mind, the largest goal is to help bridge the gap between normal programming and metaprogramming to make metaprogramming just a particular case of normal programming.

## Catln meta programming

#### Tree structure

There are a number of reasons that make Catln unusually effective at metaprogramming. The first is the consistent tree structure. As all data and functions are formed from the typed-named tuples, all data naturally has this tree structure.

Function definitions can be thought of like rewrite rules that match against a certain tree pattern and replace it with a new tree. Other features such as internal function definitions and internal value definitions are just syntactic sugar for tree rewrite rules. They are duplicated to create the function tree and then later de-duplicated during compilation with a Common Subexpression Elimination pass. This means that the metaprogramming can also be described with these same kinds of tree rewriting rules. Even cases where values must be reused such as IO are desugared into anonymous interior functions that share the values using function argument insertion.

In an imperative language on the other hand, you also have to deal with other structures such as memory, variables, loops, and control flow. You would need to track what variables exist and where they were created. In compilers, they usually convert these to an immutable form called Single Static Assignment (SSA) just to manage the complexity. But, metaprogramming has to either operate without effectively managing the complexity on the raw text form or after converting it into the SSA form most users would not be familiar with.

A pure immutable tree on the other hand can effectively manage the complexity as is. Variable reuse is only done through arguments. Conditions and branching only happens when deciding which function rewrites to apply (after desugaring). Loops only use higher order functions or recursion. And, the metaprogramming would just apply recursion to the recursion which should just work.

#### Homoicionic

The next reason is that the language, like lisp, uses an identical structure for both data and functions. This property is known as homoiconicity. This means that all of the functions and familiar tools within the language can be used to manipulate the functions as are used to manipulate the data.

Many languages have relatively weaker homoiconicity where they have some kind of Code data type. Then, code is just converted into a code data type or code is executed. In Catln, functions have arguments applied one at a time like data, be used in a pattern matching expression, or assembled into classes of functions. You can even have type properties for a function data type.

Lastly, it makes it easy to operate not just on a particular function call but on the data being given to the function call. So, you could have metaprogramming rules that use only part of a functions input rather than the whole thing.

#### Quasi-quoting

Unlike lisp, Catln does not require the use of quoting and unquoting. When the structure of code and data is the same, quoting is used to inform the compiler when something should behave as code and when it should behave as data.

While this seems reasonable, the process doesn't scale. It will work when one metaprogramming is being applied at once, but breaks down if multiple need to be integrated. For example, you may want to compile your program but still apply all of the simple optimization rules as well.

Instead, Catln uses the type inference process to determine when functions should be applied. This includes both normal functions and metaprogramming ones. Because of this, it is much easier both to call metaprogramming and integrate metaprogramming with normal programming.

#### Choice

This does leave Catln compilation to behave in a way which is unpredictable and almost nondeterministic. However, problems are fundamentally nondeterministic with multiple possible solutions. Instead of running away from this and trying to force determinism, it is much simpler to leave it naturally non-deterministic. Then, provide the tools to effectively manage the nondeterminism.

This concept is referred to as [choice](choice.md). In the area of metaprogramming, I tend to think of it as changing the goal from defining what **should** be done to what **can** be done. One of the most difficult things is not the process of applying various rules, but to know whether they should be applied to begin with. Choice allows these two problems to be completely separated. It makes both tasks significantly easier to understand and implement. Most importantly, it is able to grow to add new possibilities afterwards. So, metaprogramming doesn't need to worry about the nondeterminism and can rely on the choice to resolve it.

Choice is a problem that exists everywhere in programming, but some of the most troublesome areas are in metaprogramming. This is due to the fact that metaprogramming combines one domain of knowledge with an arbitrary domain of knowledge. Usually, the the calling domain knows what it is calling. But here, neither domain knows anything about each other.

Consider the autoCloud from single server example. Let's say that somewhere in the single server code, which I will refer to as the business logic, you have a map applied to a list. There are actually three ways this can be handled. First, the map can be applied in a single thread. Second, the map can be applied by multiple threads on a single machine. Third, the map can be divided across an entire cluster of machines in the cloud. The metaprogrammer writing the autoCloud won't know anything about the business logic so they won't know what map to use. The business logic shouldn't know about the cloud to know what map options are available. So, there is no place to put the desired type of map without choice.

Another example of this is that it is also not necessary to distinguish during metaprogramming which code should be executed during compile time and which should be executed during runtime. I am not saying that this is not a problem, but that this is not a problem specific to metaprogramming. The same thing applies to other code such as pre-computing constant values, constant branching, or pre-computing a `printf` expression with a constant string. Instead of having a partial solution that only applies to metaprogramming, it is better to leverage a more global solution with choice. So, the metaprogramming should be abstract over this choice as well.

#### Type system

Through all of this, one final note is that the metaprogramming code still behaves quite naturally. It all uses the same type system that helps prevents errors. All the code is still valid code, not arbitrary strings, and would just use anonymous functions for inserting arbitrary behavior. It follows a normal format for testing, documentation, and examples. And, it doesn't even require special syntax or features.

### Metaprogramming Format

In Catln, the mechanism for meta programming is a multi-level function definitions (see [basics](basics.md)). The usual format of application is `metaprogrammingFunction(function(functionArgs...))`. Depending on the task, some metaprogramming tasks can be implemented with just standard definitions in this fashion. Others will need harder to represent arrows that use the specialized [macro capabilities](macros.md).

This is also interesting because it falls naturally within Catlns model of computation. Imperative programming and functional programming are both limited by the idea that code is always evaluated. Metaprogramming is essentially that code can be more than just an evaluation target. So, input arguments that are multi-level and contain metaprogramming are part of the "tuple substitution" nature of the language. There is no need to change it or expand it to handle this problem. This is why metaprogramming fits with the standard programming and the existing type system.
