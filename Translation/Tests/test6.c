// To test factorial program 

int factorial (int n) {
  if (n == 0) {
    return 1;
  } 
  return n * factorial(n-1);
}

int main() {
  int n = 10;
  printf("Factorial of %d = %d\n", n, factorial(n));
  return 0;
}