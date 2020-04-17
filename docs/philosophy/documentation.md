# Documentation

Documentation is important for developing a scalable language. Documentation exists as a goal in several different ways:

## Code Documentation

Code documentation is probably the most well established. This includes documenting types, variables, methods, parameters, and return values. Documenting all of these ideas could help improve the understanding.

However, there are mixed results of this. Natural documentation exists in the form of naming and type signatures. Naming requires a balance of typability and descriptiveness, so documentation can help pick up some slack there. But, factors about the types should be expressed formally using the signature rather than informally in documentation when possible. In that respect, I am hesitant to require this documentation as it could encourage users not to devote the proper attention to names and signatures.

Another tricky aspect of this documentation is the independence of these docs. In reality, many separate pieces of docs are in fact related. They could link to each other, but transclusion would be better.

## Relationship Documentation

Another aspect of documentation exists in the relationship between entities (types, methods). If they are collocated in a file, then there is a natural location for this documentation. Some appropriate link to guide users to the documentation would be necessary as well. At that point, the system starts to resemble literate programming.

For concepts that are higher level, there must be a place to include this documentation as well. Typically, it is just a folder of files that can be structured depending on the need of the program. However, users would then need to be able to find those files when it is necessary. This can be accomplished with linking - both from other documentation and from error messages.

## Examples and Tests

Another set of documentation exists within other code like examples and tests. In order to keep either of these up to date, a system should be able to execute it as part of CI/building. If these code snippets wish to interact with each other, they would also need to be able to import each other.

However, some examples might require inputs that are better left off or might wish to ignore irrelevant parameters. In this case, they are impossible to run. Some kind of rudimentary typechecking, if possible, might still be possible to help recognize out of date examples.
