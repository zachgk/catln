# Arrow Testing

One of the most important ideas in the language is arrow testing. The general idea behind arrow testing is that when there is any kind of overlap in definitions, they must have equal results given the same inputs. This can be verified either using property testing and automatically providing inputs to the function or through theorem proving methods.

The simplest use case for arrow testing is inheritance. In Java for example, you can override a method in a subclass compared to the base class. However, the overrided method can differ in behavior from the base class which means you can never be sure that the behavior of method does not change. Arrow testing, however, requires that the overrided method must always return the same result. This means that the overriding is only useful when there is a more performant way to calculate the result in the child (although it would be more and less specific types here).

This is even more important in the case of multiple inheritance. The typical problem is that it is entirely unclear in the case of both parents having the same method which one should be used. Arrow testing guarantees that they must be equal so either choice would be equally valid. In this case, some simple heuristics can be used to try to select the best choice or it can be manually specified.

Another benefit of arrow testing is that various methods can be tested by providing multiple definitions. For example, a more complicated fib method can be combined with the simpler variation to verify that the complicated one is still correct. It could also be used (along with an annotation to indicate tests) to write simple unit tests instead of a simple testing framework. Lastly, it can be used to describe invariates such as the Functor laws to help verify that class instances are correctly defined.

## Implicit Arrow Testing

But, the greatest benefit of arrow testing comes about in combination with implicit conversions. Similarly to category theory, any two paths going from the same start node to the same end node must be equal. So, the conversions themselves can be converted by following different conversion paths to the same result. It can also test functions. For any combination of `implict -> method -> implicit` that can get the same type, it should be equal and can be tested.

As an example, consider a list type. You can have multiple kinds of lists such as an arrayList or a linkedList. Since they are both lists, you could implicitly convert between the two types of lists. So, if you start with a value in an arrayList, convert it into a linkedList, and take the length, it should be equivalent to taking the length directly. Like this, you can use the comparison to test almost the entirety of the list class and the methods within it. It is basically a free test suite, although additional tests may still be beneficial (although those additional tests would only need to test one kind of list and would be automatically applied to all).

Another major benefit is also introduced from this. It helps ensure that the APIs are consistent. For example, you could have an implicit between an Optional type and a List type where the list uses zero elements to represent Nothing and a singleton list to represent a value. If both these types have a map function, it would mean that their map functions must be equivalent. This is true even if the implicit is only one way (Optional -> List) as that direction can be tested even if the reverse is not.

## Implementation

Imagine you have a multi-level tuple `++('a', ++('b', 'c'))`. Then, you are able to follow implicit rules and substitute the implicit rules for various sublevels of the tuples. Then, you could evaluate it to:
```
++('a', ++('b', 'c')) -> ++('a', "bc") -> "abc"

++('a', ++('b', 'c')) -> concat(['a', 'b', 'c']) -> "abc"
```

Essentially, apply all possible implicit rules nondeterministically to find all possible values that would be generated during the evaluation. For all of the generated values (including initial, intermediate, and final values), all ones with the same tuple types at all levels must be equal.

An easy way to test this is to find a map from the value types to the produced values. Then, check to see that the produced values for each type are all equal. If not, show an informative error to the user.

Given this implementation to execute arrow testing, the remaining problem is how to generate these multi-lelvel tuples. There will probably be different areas to generate for different situations.
