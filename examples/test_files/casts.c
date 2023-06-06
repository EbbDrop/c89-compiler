//output:
//4
//4.700000
//8
//output-mips:
//4
//4.699999809265137
//8

#include <stdio.h>

int main() {
    int a = (int)4.3;
    float b = (float)a + 0.7;
    char c = (char)((float)a + b);
    printf("%i\n", a);
    printf("%f\n", b);
    printf("%i\n", c);
}
