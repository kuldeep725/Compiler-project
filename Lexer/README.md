## Lexer 

* This is lexer phase for tokenizing the C program (source language).
* In this phase, only the lexing of the input file (in our case, c program) is done. Basically, this will generate the tokens which will later be parsed to the parser. 
* To verify the proper lexing, syntax highlighting is also done to see the effect of lexing.

## What's done 
* Almost all the keywords in c 
* identifiers
* multiline comments, singleline comments
* directives (#include and #define)
* almost all types of values in c including 
	* int
	* char
	* float
	* long
	* double
* all kinds of seperators in c 
* all kinds of assignments in c 
* all kinds of operators in c
* strings 

## What's left 
* May be some new keywords

## How to run
All test files for Lexer are present in Lexer/Tests folder

**To run three files test1.c, test2.c, test3.c present in Tests folder** 


./driver Tests/test1.c Tests/test2.c Tests/test3.c
