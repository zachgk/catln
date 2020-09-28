# Testing

For writing secure, reliable, and maintainable software, testing is a necessity. It can be used to check your code does not have bugs and to prevent bugs from appearing when updating and refactoring the code. However, testing is also the subject of numerous blog posts, debates, and discussions. They argue the relative merits of unit tests and integration. The importance of test coverage, mocking, and test driven development. I will describe my current thoughts on testing and how I plan to implement it with Catln here.

## Testing Fundamentals

Think of each function as having a domain, co-domain, and a map from each element of the domain to an element of the co-domain. The code function is then an algorithm that produces this relationship. Tests provide a redundant mapping from domain to co-domain to be used to check if they match.

A unit test and an integration test represent specific elements in the domain and manually verify they produce the correct items in the co-domain. So, it will test only a few inputs to the function. Ideally those inputs are chosen with purpose. In unit tests, the purpose is variety. In integration tests, the purpose is that it matches real use cases.

However, it is unreliable for humans to decide the appropriate elements from the function domain to test. There are other alternatives. The most common alternative is known as property testing. In property testing, you describe a property of the function that should be true for all inputs. For example, a property of appending to a list is that the length of the list increases by one. This property is true, and could be verified, for all types of lists, lengths of lists, contents of the lists, and elements that are being appended. Then, the testing Engine can accept the responsibility of choosing items from the domain to test as well as ensuring code coverage.

The one potential issue with property testing is that the choice of properties may not fully verify correctness. In the example of appending while only checking the length, numerous incorrect (if contrived) problems are possible such as inserting the wrong element, messing up the rest of the list, and changing the types. None of these would cause the property test to fail. Formally, the property testing restricts the co-domain and then verifies that the values produced are in the smaller co-domain. A co-domain of 1 would perfectly test the values, but larger co-domains will not. In the appending example, we could perfectly property test it by ensuring that the last element is the one appended and the preceding elements are the original list. Regardless, property tests greatest strength is how little work is required to create them.

Another potential avenue for testing is by creating multiple definitions of a function. Those definitions should then match for all inputs in a domain. Duplicate functions are the best form of testing as they can fully verify across the domain and fully verify each element in the domain. This idea is a part of [arrow testing](arrowTesting.md).

## Testing Implementation

Writing tests requires only a few simple operations: the test/example annotation, the assertion, and a testing engine. The testing behaves most similarly to property testing so any function that has [assertions](assertions.md) inside it will be subject to testing.

You can put the assertions directly into the implementation of functions for property tests. If you want to test specific cases (unit or integration tests), you should create a global value for the test, give it a `#test` (or `#example`) annotation, and include the necessary assertion inside it. For having tests that are randomized, just use a global function instead with the same annotation and the randomized inputs as arguments.

```
testAddDouble(Int x) = 
  #test
  assert(test=x + x == 2 * x)
```

You can also create a simple unit/integration test case by simply creating an overlapping function definition. For example, you could write:
```
#test
  operator+(1, 1) = 2
```

If you have duplicate functions, no additional work is necessary. They will be run automatically as [arrow tests](arrowTesting.md).

### Testing Engine

In the actual testing process, all tests really align into two ideas. Any function which has an assertion should be verified by property testing. This is used to execute all of the complicated unit/integration tests too.

Then, any test with overlapping definitions is executed using [arrow testing](arrowTesting.md). The simple tests written as sample function inputs and outputs just use arrow testing (with the domain of one item) to match against the other definitions.

One requirement of this system is the ability to create sample function inputs. If you come from an imperative background, this may seem somewhat unreliable. Generally, that means that you are not making a strong enough use of the type system to encode the fundamental ideas behind your data types. You should write it so that any invalid combination of data would not be allowed in the object and match the type definition. The reverse of this is then any type creation would be a valid input. Furthermore, all of the edge cases should be much easier to create as there are either boundaries in numbers, or cases in the sum types.

### Test Proofs

In lieu of using the testing engine to produce sample inputs to a function, it may be possible to prove using the type properties that an assertion is guaranteed to be true. This would actually be proven during type inference, so in those instances no tests need to actually be run. You can also specify that an assertion must be proven for a stronger verification.

### Test Caching

In order to have a good development experience, it is important for the tools to be as fast as possible. That makes it easier and more convenient to run them more often. This is also an important piece in testing. With the heavy use of arrow testing and property testing, the tests might be generally slower than most testing systems.

In order to improve the performance of the testing, the tests can be cached. If no input functions to a test have changed, the tests do not have to be rerun and the result can be pulled from the cache. Given this, it is far faster to run an entire test suite without worrying about wasting time.

### Testing Difference Engine

With testing, one goal is to avoid accidentally changing the behavior of a function. To suit this goal, it requires an extensive test suite with high coverage. This test suite requires significant manual work to create an upkeep. Instead, a tool can do it almost effortlessly.

This powerful addition to the testing system is the use of a testing difference engine. The difference engine expects you have a copy of the code in two different files (or directories). It tests the two implementations against each other using arrow testing to ensure that matching functions should be equal for all possible inputs. When it isn't, the engine should (as always) attempt to find the simple examples that demonstrate how it changes.

The most useful application of this difference engine is for testing over time. If you have a pull request, you can view the difference before and after the changes. This will help the reviewer identify what functions have changed and how. Without this, it often requires that those changes are not systematically detected due to no tests or that the PR author manually updates all of the relevant tests to the new definition. Instead, the use of the difference engine merely requires enabling a tool.

### Coverage

When the tests are run, it will automatically record the code coverage as well. The coverage will be recorded using only a single metric of blocks (essentially conditionals covered) instead of by line. For each block, it will also be recorded whether the test was complete (arrow testing or perfect property testing) or incomplete (unit testing or imperfect property testing). It will also record which tests cover each line of the code as well.

## Guide to Writing Tests

### Unit Testing, Integration Testing, and Examples

One of the first, and largest, areas of testing is unit testing. Some plan for many unit tests while others consider them too tedious. Overall, I believe that there are often better tools to accomplish the goals that unit testing often handles.

First, unit testing is sometimes used to ensure correct behavior on edge cases. For this purpose, an effective type system should be better. With a bounded sum type, it is impossible to forget to handle one of the cases without a type error. Likewise, by making properties that are optional clear in the type system, they also can't be missed. Instead of relying on manual tests, automatic types require no work and will do a more thorough and faster job of ensuring that most corner cases are handled.

Another goal is to get good coverage. With the testing difference engine, the coverage is not as essential of a metric. Furthermore, using property tests is a much more effective way of increasing the test coverage

In the [documentation page](documentation.md), I describe the way of organizing code as if you are writing a book to explain the details of your library/service. In that respect, you should write manual tests for cases that you believe have explanatory value to your readers. They should illustrate useful cases to understand how the function works and usually be co-located with either the function or it's declaration. As such, there is not a lot of difference between unit tests, integration tests, and examples which is why I have combined them together.

The only major difference would be the use of mocking. As a pure language (outside of IO), you should not need to use mocking besides a MockIO implementation. The MockIO should be reserved for only the functions that interact with the user directly while most of the logic should be located elsewhere.

### Arrow Testing

Arrow testing should be the most convenient form of testing as it requires little additional thought. Most of it would be automatic based on definitions.

The only time to consider arrow testing is when you are designing algorithms with thought of their runtime. While you obviously want to create the faster versions, also consider writing the simpler native version to better explain the function. This then doubles as an arrow test which will verify the more complicated implementation as well.

### Property Testing

The bulk of your testing should be done in property testing. However, the bulk would only be in terms of value instead of cost. When you write functions, try to include assertions as well. These can be in terms of the final value or intermediate values as well. The assertions can also provide a useful avenue to help provide additional explanation to those reading your function as well.
