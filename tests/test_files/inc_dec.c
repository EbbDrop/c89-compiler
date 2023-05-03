//output:
//3
//4
//5
//4
//4
//5
//4
//3

#include <stdio.h>

int main() {
    int a = 3;
    printf("%i\n", a++);
    printf("%i\n", a++);
    printf("%i\n", a--);
    printf("%i\n", a--);
    printf("%i\n", ++a);
    printf("%i\n", ++a);
    printf("%i\n", --a);
    printf("%i\n", --a);
}
