//output:
//0
//1
//0
//1
//0
//0
//1
//1
//0
//0
//1
//1
//1
//0
//1
//0

#include <stdio.h>

int main() {
    printf("%i\n", 1 > 3);
    printf("%i\n", 3 > 1);
    printf("%i\n", 3 > 3);
    printf("%i\n", 1 < 3);
    printf("%i\n", 3 < 1);
    printf("%i\n", 3 < 3);
    printf("%i\n", 1 <= 3);
    printf("%i\n", 3 <= 3);
    printf("%i\n", 3 <= 1);
    printf("%i\n", 1 >= 3);
    printf("%i\n", 3 >= 3);
    printf("%i\n", 3 >= 1);
    printf("%i\n", 2 == 2);
    printf("%i\n", 2 == 4);
    printf("%i\n", 2 != 3);
    printf("%i\n", 3 != 3);
}
