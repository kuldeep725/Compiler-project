import java.util.*;

public class Test5 {

   private static int a = 10;
   public static int reduce (int b) {
      int d = 10;
      if(b <= 0) {
         return b;
      }
      return reduce(b - 1);
   }
   public static void main (String[] args) {
      int b = 10;
      int c = a + 10;
      int r = reduce(b);
      int reduce;
      char ch = Character.toUpperCase('c');
      if(!(c != 0)) {
         int k = 100;
      }

   }

}