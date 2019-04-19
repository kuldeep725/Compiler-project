#include<stdio.h>
#include<ctype.h>

int a = 10;

int reduce (int b) {
  int d = 10;
  if (b <= 0) {
    return b;
  }
  return reduce (b-1);
}

int main() {

  int b = 10;
  int c = a+10;
  int r = reduce (b);
  int reduce;
  char ch = toupper('c');
  
  if (!c) {
    int k = 100;
  }
  // r = k;
  return 0;
}

