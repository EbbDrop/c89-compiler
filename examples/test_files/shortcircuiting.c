//output:
//3
//0
//2

#include <stdio.h>

int main() {
    int a = 3;
    printf("%i\n", a);

    (a = 0) && (a = 255);

    printf("%i\n", a);

    (a = 2) || (a = 255);

    printf("%i\n", a);
}
