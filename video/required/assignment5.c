// • Assignment 5:
//   – Function scopes
//   – Symbol table for function scopes
//   – Local and global variables

#include <stdio.h>

// Global variables
int a = 3;
int b = 4;

void function_scope(int b) {
    int a = 1;
    {
        int b = 2;
        printf("local a = %i; local b = %i\n", a, b);
    }
    printf("local a = %i; param b = %i\n", a, b);
}

int main() {
    printf("global a = %i; global b = %i\n", a, b);
    function_scope(9);

    int a = -2;
    printf("local a = %i; global b = %i\n", a, b);

    {
        int b = -6;
        printf("local a = %i; local b = %i\n", a, b);
    }

    b = 0; // set global b to 0

    printf("local a = %i; global b = %i\n", a, b);

    return 0;
}
