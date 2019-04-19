#include<stdio.h>

int a = 10;

int main() {

  int b = 10;
  int c = a+10;

  if (c && b) {
    /* code */
    printf("Inside if block = %d\n", c);
  }
  
  return 0;
}
