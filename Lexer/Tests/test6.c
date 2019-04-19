// Testing function prototypes
#include <stdio.h>

/* function declaration goes here.*/
void print_swap( int p1, int p2 );

int main()
{
   int a = 10;
   int b = 20;

   printf("Before: Value of a = %d and value of b = %d\n", a, b );
   print_swap( a, b );
}

void print_swap( int p1, int p2 )
{
    int t;

    t = p2;
    p2 = p1;
    p1 = t;
   printf("Value of a (p1) = %d and value of b(p2) = %d\n", p1, p2 );
}