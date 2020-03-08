# Assertions

In many languages, the source files and the test files are typically kept separate. With arrow testing, it is possible to have a reasonable amount of testing integrated naturally into the code.

To expand on this, assertions can be integrated into the code as well. Unlike most imperative languages, the assertions should only be recognized during testing as opposed to during runtime. But, the assertions can then be used for property testing which further expands the testing possible without writing a separate test suite. Even in larger integration testing, they can also be tested in order to cover additional cases.

The other key benefit of the assertions is that they can serve as a key input with respect to type properties. If the assertion is true, it could imply various type properties with respect to the variables used in the assertion. This feature would already exist in the case of conditional statements such as `if(x > 5)` means that within the true branch, we can assert that `x > 5`. Even in other branches, we know the opposite that `not(x > 5)     ==>     x <=5` .
