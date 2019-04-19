// This program is trying to use a variabe not declared
// This should give error 
#include<stdio.h>

int a = 10;

int reduce (int b) {
  int d = 10;
  if (b > 1e5) {
    return b;
  }
  return reduce (b>>2);
}

int main() {

  int b = 10;
  int c = a+10;
  c = d;
  int r = reduce (b);

  return 0;
}

