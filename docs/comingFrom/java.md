# Coming From Java

This is a short guide of things to keep in mind when using Catln for programmers from a Java background.

## Inheritance

One of the first differences when seeing Catln is that it doesn't match up exactly with the idea of inheritance. However, Catln uses different language but can achieve most of the same effect. However, there are two key areas to keep in mind.

First, it resolves ambiguities with regard to classes. Imagine you have the following two classes:

```java
public class Parent {}
public class Child extends Parent {}
```

Let's say that you have a `Parent` object. This is actually ambiguous because there are two different meanings. You could be referring to the group of classes that are `Parent-like`, or the specific member of the group which is the `Parent-exact`. Without different ways to refer to these different concepts, code can't tell whether you want the group of classes or the exact class.

To handle this situation, there is a theory of Covariance and Contravariance. However, Catln resolves it by adding an additional restriction compared to OOP: all classes must be abstract or final. Then, it is clear.

In Catln terms, both Java interfaces and Java abstract classes can be implemented using Catln classes. Java final classes can be implemented as Catln `data`.

The second restriction comes from the use of overloading methods. In Catln, every function is considered to be a true statement. If it were allowed to overload a function, it would mean that the function may or may not be true. This is fairly useless as far as statements go.

So, overloading is only allowed when both definitions are true (and usually equal for all function inputs). For example, imagine you have a set and want all the values in a range. For any set, you can list all the elements and filter to only the ones in the range. If it were a tree set, you could use tree operations to find the range. But, the general set operation would still work even though it would be less efficient.

In cases where you need behavior which can be overrided, consider putting the default behavior in a supplementary class. Users can extend the supplementary class to get the default behavior or extend only the main class to get their own behavior.
