// This test includes INT_VAL, FLOAT_VAL, CHAR_VAL also
#include <stdio.h> 
#define MAX_VAL 10000

/* Main begins here 
    Here it goes*/
int main () {

    int a, b;
    char c;
    float d = 2.2e-2; 

    int e = 10;
    char f = 'a';
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