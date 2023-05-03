//output:
//4
//6
//9

#include <stdio.h>

int main() {
    const int a = 4;
    const int b = a + 2;
    const int c = ((b * 23) / 2) % 10;

    printf("%i\n", a);
    printf("%i\n", b);
    printf("%i\n", c);
}
