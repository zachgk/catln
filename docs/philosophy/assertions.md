# Assertions

In many languages, the source files and the test files are typically kept separate. With [arrow testing](arrowTesting.md), it is possible to have a reasonable amount of testing integrated naturally into the code.

To expand on this, assertions can be integrated into the code as well. The effect of an assertion can be assumed to limit the domain of a value, often relative to other values and inputs within the function. There are two major purposes to this.

First, the assertions are used for testing. In a specific test function, it would act like a normal unit test or integration test. When integrated into the main code functions, it behaves like property testing. Those tests can run one of two ways. If it is possible to prove the assertion using the type system's type properties, it is the best assurance. Otherwise, a testing Engine can attempt to provide inputs that are random and edge cases to verify that the assertion is always true. See the [testing document](testing.md) for more details.

The other key benefit of the assertions is that they can serve as a key input with respect to type properties. Because the assertion is true, it could imply various type properties are true as well. This feature would already exist in the case of conditional statements such as `if(x > 5)` means that within the true branch, we can assert that `x > 5`. Even in other branches, we know the opposite that `not(x > 5)   ➜     x≤5` . These properties can then be used for better type inference and a clearer return value.
