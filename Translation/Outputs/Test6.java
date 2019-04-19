import java.util.*;

public class Test6 {

   public static int factorial (int n) {
      if(n == 0) {
         return 1;
      }
      return n * factorial(n - 1);
   }
   public static void main (String[] args) {
      int n = 10;
      System.out.printf("Factorial of %d = %d\n", n, factorial(n));

   }

}