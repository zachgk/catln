# catln

Catln is a programming language designed to be very high-level and very strongly typed.

**The language is currently under development. Only some of the critical language features have been implemented in the compiler so it is not possible to write code within the language yet. However, those interested can take a look at our [documentation](documentation) which describes in more detail the goals, philosophies, and implementation plans behind the language or message [@zachgk](mailto:zachary@kimberg.com).**

### High-Level

There are a wide variety of high-level languages that have different criteria for what high level means. The criteria I use is to separate information in a function into information that affects **what** the result is and information that affects **how** to get the result. The information affecting **what** include which subfunctions are called with what inputs. The information affecting **how** include what data structure implementations are used (array list vs linked list), memory types (stack vs heap), memory reuse, parallelization/threading, strict/lazy, static/dynamic dispatch, caching, etc. 

The goals set as a high level language is to focus entirely on the question of what and make all of the details regarding how **impossible to express** in the language proper. The goal is to free the user from any possible concern or ability to affect performance while writing the code. They can focus only on essential complexity of their problem domain and avoid introducing additional complexity for performance. The compiler can then figure those performance details out itself.

For areas where performance is more critical, there will be supplementary annotations or configurations that can be used to specify information regarding the how question. Then, users can focus only on performance, easily try different things out, and be guaranteed that they can't introduce any bugs as they are not affecting what is returned.

### Strongly Typed

The other goal of the language is to have strong typing. Typing as well can have somewhat different meanings. In a low-level language, the typing exists closer to memory and is used to determine what registers and CPU instructions to use. Typing in some languages can allow you to introduce some abstractions to organize your code and type checking to help convert runtime errors into compile time errors. At higher levels, having the typing more closely reflecting the underlying problems can make it even more difficult to introduce bugs.

In this, the goal of a type is to represent things that you know before your code is run. While the actual values may not be known, some information about it can still be determined. The type can be thought of as the set of all possible values and the goal is to narrow down the set. For example, the set of integers is better than the set of numbers. But, the set of integers that are greater than or equal to zero is even better. This additional information can be used to determine when functions can be called, used to find compile errors, and used for optimizations. Once this is known, a programmer should try to represent everything they know about the program's problem domain using types.
