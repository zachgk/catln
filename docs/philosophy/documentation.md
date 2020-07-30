# Documentation

Documentation is important for developing a language for maintainable code. Documentation exists as a goal in several different ways:

### Code Documentation

Code documentation is probably the most well established. This includes documenting types, variables, methods, parameters, and return values. Documenting all of these ideas could help readers understand the code being written.

However, there are mixed results of this. Natural documentation exists in the form of [naming](naming.md) and type signatures. Naming can help give guidance about the relative purpose of everything. The reason Catln focuses on named tuples rather than positional arguments is precisely to increase the amount of names to add natural description to the code.

The other factor is the usage of types. Having a precise and descriptive type system allows a significant amount of information and relationships about code to be given. At all times, it is better to prefer the compiler comprehensible use of types rather than informal ideas only passed in documentation. Due to the power of these signatures, I am hesitant to require documentation about things like methods and return values as the signature often presents that information even more clearly.

Another tricky aspect of this documentation is the independence of these docs. In reality, many separate pieces of docs are in fact related. They could link to each other, but including would be better.

### Other

However, it is often easy to miss the other important kinds of documentation besides the code documentation. First, it is often useful to have documentation for a package to describe what broadly speaking is located inside that package.

Another set of documentation exists in the case of examples. Examples help describe to readers the best ways to use the code. These ideas can serve to introduce sections and to summarize them. It is also an important piece to guide users to.

I would argue that tests are also a case of documentation. Most tests that are written should be property tests and [arrow tests](arrowTesting.md). That leaves the remaining cases of integration test and unit tests. An integration test should behave similarly to an example, otherwise it doesn't have a lot of utility in terms of describing behaviors users are not expected to do. Unit tests should exist to explain and enforce important properties of the code. If those properties are not even worth describing to readers, they are not likely important enough to deserve a test. See [testing](testing.md) for more details.

Finally, there are also documents to explain the cohesive theory behind the code. For others who are learning about it, there are many important descriptions of not just what code exists but why the code was created that way. Without a clear location for this information, much of it is only known by the developers who work on it and eventually lost.

## Documentation in Catln

In Catln, I think of documentation as fundamentally intertwined with the idea of code organization. Imagine that you were writing a book about your library. You want to organize it in the clearest order to explain each of the key concepts. For your book to be sufficiently precise, it would essentially need to include the code to help formalize all of the ideas described. If your book also has all of the code, then it means that your code could be entirely found within this documentation book.

One of the trickiest parts about documentation is to keep it in sync with the code. While no system is perfect, I would argue that the most important aspect is to have them kept as close together as possible. It is far easier to recognize bad method documentation than a document nobody ever sees.

In this book format, each file should correspond to one "page" of documentation. A section of the book would then be a subdirectory or package of code. Unlike a book, these sections will probably be a thinner and deeper tree (fewer chapters but more hierarchy in each). Each directory would have a main file that would provide a description for the section of the book as a whole and can also include the order of the files/subdirectories within.

In reality, the book would also exist on two levels: public and private. The division is pretty simple that everything not marked private (see [modules](modules.md)) would be in the public book and both public and private alike would be in the private book. For this reason, it will be possible for documentation sections, tests, and examples to all be marked private.

The purpose of the public book is for consumers of the library. They are only interested in the details necessary for them to use it and information not likely to help them should be marked private. The private book should be directed as maintainers of the library who need to understand additional details. Because this should be a strict superset of all the details a consumer would need, all the public sections should be just as relevant in the private book.

In addition to the main book, there will also be collated pages for various scopes. These would include all of the methods on the scoped type, functions in the scope, and a list of subtypes with their relevant subscopes. Each of these sections would link back to their place in the book combining the key explanations in a desired order with a better ease of reference. This section is more similar to the documentation that exists now like Javadoc or Doxygen.

### Format

In a particular file, it consists mostly of documentation/comment blocks and code. The code is written as expected. All comments will be treated as documentation in the markdown format. This can be either a single line or an indented block. The comment can be prefixed with the private keyword to mark the section as private. Code blocks within the markdown will be assumed to have catln code and will be treated the same as an example.

A documentation section should be separated with newlines before and after if it is an independent section. If the documentation is specific to a particular definition, it should be located directly before the definition without a line break separating the two. These documentation pieces will then be collated into the type summary documentation as the accompanying description.

It is also possible to have these comments and documentation sections within the bounds of a function definition.

For example sections, they will be put within an `#example` annotation block. The examples will always be type checked, however they will also have several kinds of additional syntax. It can replace sections of code with a `...` and miss key definitions. It can also use values without defining them. If an example is runnable, then it would be run as part of testing. If not, it will still be parsed and type checked to the greatest possible extent to help ensure that the examples are kept up to date. Within the same file, all examples will be considered as in the same code file (alongside the actual code). So, values created in one example can be used in the others. Values will have to be named to avoid conflicts.

Similarly, tests will be inside a `#test` annotation block. Tests must be runnable without particular syntax. A test behaves the same as any other definition or declaration. They are run using the same mechanism as [arrow testing](arrowTesting.md). The only key difference is that they will show up highlighted in documentation and will be filtered out during compilation.
