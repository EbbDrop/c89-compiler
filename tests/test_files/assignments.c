//ignore
//output:
//7.000000
//3.000000
//4.000000

// TODO: stop ignoring once varargs with floats are fixed

#include <stdio.h>

int main() {
    int a = 4;
    float b = 3;
    float c = a + b;
    printf("%f\n", c);
    printf("%f\n", b);
    b = c - b;
    printf("%f\n", b);
}
