//output:
//4
//4
//8

#include <stdio.h>

int main() {
    int a = 4;
    printf("%i\n", a);
    int* ptr = &a;
    int** ptrptr = &ptr;
    printf("%i\n", a);
    **ptrptr = 8;
    printf("%i\n", a);
}
