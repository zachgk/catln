# Generic Type Usage

In many of the simpler high level languages, they reduce the number of possible data structures to use. For example, python only uses a single list type and even has a dedicated syntax for it. They do the same with the set type and the map type. This does not allow the choice between various options such as LinkedList vs ArrayList or TreeMap vs HashMap. In that case, users will simply get poor behaviors when they do certain things.

The other option is to force users to always make the choice themselves between these options. But, that introduces some mental overhead and users may not always make the best choice in the case of rarer data types.

To solve this, I believe languages should try to support generic type usage. For example, a List would be declared without specifying what kind of list it is. As long as the lists have implicit conversions between each other, there should not be incorrect results returned. The only decision is that the compiler must choose which list type(s) to use and where. While making perfect choices may not be possible, heuristics could at least return better results than a naive choice.

A simple solution would be to add a compiler annotation to discourage use of certain methods. For example, accessing a sorted list of keys in a hashmap should be discouraged and might indicate that a treemap is correct instead. List concatenation with ArrayLists could also be lightly discouraged such that if it occurs with great frequeency, you may want to use LinkedLists instead.

It could also use some actual computations of the costs of particular methods based on the inputs. Or, it could be measured experimentally during property testing the runtimes of the methods with various inputs. Lastly, it could attempt to prove the Big O values of different methods and decide based on that. Worst case, there could be a default of the various types specified by a compiler annotation and it can greatly lean towards the default or just always pick the default (especially in the beginning).
