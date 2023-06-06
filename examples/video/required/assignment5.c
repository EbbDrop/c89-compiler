// • Assignment 5:
//   – Function scopes
//   – Symbol table for function scopes
//   – Local and global variables

#include <stdio.h>

// Global variables
int aa = 3;
int bb = 4;

void function_scope(int bb) {
    int aa = 1;
    {
        int bb = 2;
        printf("local aa = %i; local bb = %i\n", aa, bb);
    }
    printf("local aa = %i; param bb = %i\n", aa, bb);
}

int main() {
    printf("global aa = %i; global bb = %i\n", aa, bb);
    function_scope(9);

    int aa = -2;
    printf("local aa = %i; global bb = %i\n", aa, bb);

    {
        int bb = -6;
        printf("local aa = %i; local bb = %i\n", aa, bb);
    }

    bb = 0; // set global bb to 0

    printf("local aa = %i; global bb = %i\n", aa, bb);

    return 0;
}
