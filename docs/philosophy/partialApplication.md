# Partial Application

The form of a data type or function call is a named tuple. Each element in the tuple can be independently set with a value for the key.

The idea of partial application is that they don't have to be set all at once. Just like tuples can be updated, the named tuples can be updated as well (including function calls). A partially applied tuple is one where only some of the keys have been given definitions and others are still missing them. For example:
```
baseFunctionCall = callName(arg1=1, arg2=2)
finalCall = match val
    True -> baseFunctionCall(arg3=9)
    False -> baseFunctionCall(arg3=2)
```

Once the value is used as the result type (determined through type inference), then the function will actually be called and the result returned. The partial application feature can be used to handle currying and many function manipulation problems.

## Partial Application Calling

One other potential usage is to be able to call the function even when it is only partially applied as long as the missing arguments are not used. This is more common in the case of data types. For example, a circle data type given only the radius but not the center location is still sufficient to determine the circle's circumference. This feature is still under consideration, though.
