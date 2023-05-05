// • Assignment 6:
//   – One-dimensional static arrays
//   – printf
//   – scanf
//   – #include <stdio.h> statement (only for printf and scanf)

int main() {
    int a;
    printf("using printf before including stdio is not possible");
    // same for scanf
    scanf("%i", &a);

    return 0;
}

#include <stdio.h>

void foo() {
    printf("here using printf is allowed, because stdio was included by now");
}

