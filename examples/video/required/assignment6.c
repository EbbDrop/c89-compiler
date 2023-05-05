// • Assignment 6:
//   – One-dimensional static arrays
//   – printf
//   – scanf
//   – #include <stdio.h> statement (only for printf and scanf)

// Includes printf and scanf
#include <stdio.h>

// Can be included multiple times
#include <stdio.h>

int x[2];

int main() {
    // Assign to array elements
    x[0] = 1;
    x[(x[0] + 1)/2] = 2;

    // one-dimensional static array
    const char *arr = "dynamic format string\n";

    printf("printf support (%i, %d, %s, %c, %f)\n", 1, 2, "three", '4', 5.0);
    printf(arr);
    printf("\nx[0] = %i; x[1] = %i", x[0], x[1]);

    return 0;
}

