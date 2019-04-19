## Translation
* In this phase, the c program is translated into java program. 
* This phase involves creation of ast for java.
* In the translation module, ast of c is converted into ast of java. 

## What's done 

* Expressions for condition in "if", "while", "ternary" is converted into conditional expressions (or boolean expressions) because java allow only boolean expressions  inside condition part of "if", "while", "ternary" 
* For evaluating expressions, boolean expressions in c (which actually evaluates to 0 or 1) are converted into integer (0 or 1) using ternary operator
* Built-in functions :
      * printf is converted into System.out.printf
      * toupper is converted into Character.toUpperCase
* File name of input file is used to create file name of output file :
      * *Input file-name* : **test1.c** 
      * *Output file-name* : **Test1.java**
* File name of input file is also used to name the main class for output java file : 
      * e.g. if input is "test1.c", then class name is "Test1" in output file
      * this makes sure condition for java which stipulates that class-name should be same as file-name
* All other c operations are translated to java operations 

## What's left
* Directives of c are ignored as they are of no use to java program
* Comments are not printed in the output file just for testing. This part can be easily done by changing one or two lines as comment-style in java and c are same.

## How to run

All test files for Translation are present in Translation/Tests folder. All output files for translations are stored in Translation/Outputs folder.

**To run three files test1.c, test2.c, test3.c present in Tests folder** 



./driver Tests/test1.c Tests/test2.c Tests/test3.c


**This will create three files Test1.java, Test2.java, Test3.java in Translation/Outputs folder.**

### Sample input c program 
> test1.c
```c
#include<stdio.h>

int a = 10;

int main() {

  int b = 10;
  int c = a+10;

}
```
### Sample output 
> Test1.java
```java
import java.util.*;

public class Test1 {

   private static int a = 10;
   public static void main (String[] args) {
      int b = 10;
      int c = a + 10;
   }

}
```