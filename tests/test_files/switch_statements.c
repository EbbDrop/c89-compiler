//output:
//3, default, end

#include <stdio.h>

int main() {
    int a = 3;
    int b;
    switch (a) {
    case 3:
        printf("3, ");
    default:
        printf("default, ");
        break;
    case 6:
        printf("6, ");
        break;
    case 9:
        printf("9, ");
    }
    printf("end\n");
}
