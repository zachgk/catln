# Language based Compilation

In order to have an effective compiler system, I would argue that as much reasoning should be pushed into the language as possible. The primary reason is that the compiler is relatively fixed. It can only be improved periodically while changes can be easily made to the code that is run.

The other key difference is that the compiler only has a relatively small knowledge about the program. While many optimizations can be made without it, plenty require it. The problematic requirement has typically been that there is no way for the language to really reason about the code. In Catln, the type properties provide a sufficient reasoning ability for much of the necessary things that compilers typically do. All that should be left are more fundamental operations based on the language system itself.

The other direction is within the area typically thought of as compiler plugins. It should be possible to develop these within the bounds of the language itself as well. Here are some of the key "plugin" directions I envision:

## Optimization Rules

Many rules for optimization can be embedded within the language itself. While many optimization rules are strictly based on the structure of the code, others require knowledge of the types themselves. The largest way to accomplish this is through [multiple level functions](typeSystem.md).

## Imports

One important feature of a file of code is to interact with external sources of information. This description sounds a bit vague because I believe it has far more power than it typically used by languages. The most typical usage is a standard code import.

The standard code import would have to either be built-in or part of the standard library. It should allow importing another Catln file into the scope of the current file. Remember that the language consists of essentially objects and arrows. So, the objects and arrows of the other file are brought into the current file.

In addition, I may add an additional import type of dependencies. These differ from a standard import in terms of the strictness. Bad formatting or warnings shouldn't be allowed within the main code, but could be excused in dependencies. They would also be excluded from coverage metrics and it may be worth skipping tests directed at dependencies as well.

Beyond this, the same idea of "importing" could be used as a general system of creating objects and arrows as prerequisites for the code in the file. For example, a static json file could be "imported" into the type system as a JSON object. Then, the contents of the file can be used while verifying through the type system that the code matches the file. It could also import things like a text file as a string or a static image to return from a web server.

Another powerful option could be to import types themselves. For example, the OpenAPI specification allows you to design an API as a JSON file. If that file were imported as a type, then the type checker could ensure that the implementation matches the API definition. Not only would it simplify the code, it would help prevent bugs and errors. The same could be used for other code generation like protobuf files.

Another avenue for importing is to handle language interop. For the simplest example of C header files with static binaries, the functions in the C header could be imported as objects. However, the arrows would all be undefined within the actual Catln except by interoperating with the code generation.

Likewise, a similar system could be used to interop with other languages as well. Swift for Tensorflow added Swift > Python interoperability to take advantage of the expansive library of python functions. The same could be used to leverage the python libraries from Catln too. However, it would be better to create wrappers as the Python language lacks type bindings.

Overall, an expandable import system would help resolve the requirements on code generation/macros from the build system while providing high ease of use to developers. It would most likely require heavy use of the [macro system](macros.md), but that should be fine.

## Compiler Targets

Potentially the most powerful area is by changing the compiler targets. It is standard to think of a program producing a single binary executable. In the current design, this executable is produced by building the system into a tree form, generating the tree into LLVM, optimizing the LLVM, and saving using LLVM.

However, there is another way to consider it. The main function of the program has the following signature `main<IO>(List[String] args) -> IO`. It we create a type `CatlnLLVMExecutable`, the final compiler stages could be described as an implicit conversion from `IO -> CatlnLLVMExecutable`. This implicit could be written within the standard library instead of the compiler.

While it is possible that the code will be more difficult, it shouldn't be too large of a difference. It shouldn't be crazy given the power of implicit conversions, multi-level functions, and macros. It should end up fairly similar to simply writing it in catln to begin with.

There are a number of benefits to this approach. First, it means that the compiler becomes extendable and additional libraries can interact with the compiler types and classes. By using [generic type usage](genericTypeUsage.md), whole compiler modules can be replaced wholesale if desired. The compiler can also be configured at a high granularity using the same systems as annotation configuration files that specify the **how** details of the code.

Another benefit is that it is possible to write multiple compilers. Instead of the standard LLVM compiler, a simple direct to x86 compiler could be made that might run faster for use during development. It can also be used to create a separate parallel compiler or even a GPU compiler to replace the standard one.

The greatest power is expanding the very nature of a program. The final result type can be expanded from a single executable to a more general `CatlnResult` class. Then, we would have `instance CatlnLLVMExecutable of CatlnResult` so that the LLVM binary would be one valid result from compiling a Catln file.

Other results could be generated as well. For building a server, maybe it would generate the server executable along with a database schema. If you are building a full-stack web application, it could generate both the client and server simultaneously. You would only need one program and one language for your application, while being able to write validation to apply on both sides and type checking to help ensure your client and server match. You could even throw in mobile as well.

It would also be possible to program distributed architectures. Then, Catln could choose (or be given) the architecture for your application and automatically integrate them into a final result package. Instead of writing the business logic within the distributed code, they can be written completely separately, utilize a common interface, and be combined later. It might even be possible to switch out different architectures to find the best one to suit your application. Beyond that, it could even generate a full cloud system as a specification for a cloud provider (AWS, GPC, Azure, etc.) written in the cloud configuration (CloudFormation, Terraform, etc).
