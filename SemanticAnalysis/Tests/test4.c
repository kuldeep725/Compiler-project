// This program is trying to use a variable not declared in its scope
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
  int r = reduce (b);

  if (c) {
    // k is declared here
    int k = 100;
  } // scope of k ends here
  
  r = k;    // this should give error as k is not declared
  return 0;
}

