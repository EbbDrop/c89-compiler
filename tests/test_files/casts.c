//ignore
//output:
//4
//4.700000
//8

// TODO: stop ignoring once varargs with floats are fixed

#include <stdio.h>

int main() {
    int a = (int)4.3;
    float b = (float)a + 0.7;
    char c = (char)((float)a + b);
    printf("%i\n", a);
    printf("%f\n", b);
    printf("%i\n", c);
}
