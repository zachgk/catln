# Debugging

Good debugging tools are essential to a good development experience. This is one of the areas where functional languages sometimes struggle. While they often have good REPLs, other things like debuggers do not work quite as well.

### REPL

One of the first tools that can be used for debugging is a REPL. Being able to easily execute a function with different inputs can both help during the code writing and also help check which inputs your function does not get the correct result for. A REPL would therefore be a good first item to include in terms of debugging tools.

### Debugger

In most imperative languages, a debugger stops at a particular line of code and shows you all the variables in scope (and possibly in your call stack as well). You can advance your debugger to the next line of code either into a function or over a function, as well as continue to the next breakpoint.

As the language is not time based, there is no need to limit it to only moving forwards through time. The best way is to view your whole program execution as a single giant tree. You can then explore the tree. Each node represents a function and the children are the variables and sub-functions called in the tree. You can then go down the tree to the child functions or up the tree to view the parents. As each function has all immutable values, all values can be shown simultaneously as they don't change during the course of the function.

Instead of breakpoints, it is equally easy to simply list all of the places in the tree where a particular function is called. The user can then go through all of them as well as skip over ones that are not interesting easier.

Another powerful option is to add a `#debug(val)` annotation. These values should be printed out when debugging begins similarly to a typical debugging print statement. However, the user can click on them to go straight to the node in the tree representing that value to combine both print statement and debugger debugging for the best value.
