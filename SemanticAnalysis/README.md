## Semantic Analysis
* In this phase, the semantics of c program is checked and verified.
* In case of some error, the program will stop running giving the error message.
* Symbol table is created in this phase to keep track of variables and functions.

## What's done
* An undeclared variable can not be used
* A variable declared in some different scope can't be used in some other scope
* Global variable can be used in any block/function after it is declared
* Functions should be declared before the call is made, i.e. the function definition should be above in terms of code before function call is made.
* Undefined functions can't be called
* During call a function, number of arguments passed to the function should be same as the number of parameters which were present when function was defined


## How to run
All test files for Semantic phase are present in SemanticAnalysis/Tests folder

**To run three files test1.c, test2.c, test3.c present in Tests folder** 


./driver Tests/test1.c Tests/test2.c Tests/test3.c
