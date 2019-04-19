// This Test contains basic types and only INT_VAL
#include <stdio.h> 
#define MAX_VAL 10000

/* Main begins here 
    Here it goes*/
int main () {

    int a, b;
    char c;

    int e = 10;
    char str[] = "Hello World!";

    if (e > 0) {
        e += 5;
    } else {
        e *= 100;
    }
    
    while (e > 0) {
        long g = (e == 10) ? 1 : 0;
        if (e % 2 == 0) 
            break;
        e--;
    }
    return 0;
}