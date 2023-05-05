// Assignment 2:
//  – Types (float, char, int)
//  – Variables
//  – Constant variables
//  – Pointers
//  – Basic pointer operations: address (&var), dereference (*var)
//  – Constant propagation
//  – Construction and usage of the symbol table
//
// Assignment 3:
//  – Single- and multi-line comments

#include <stdio.h>

int main() {
    // Variables, types (float, char, int), pointers
    float f = -1.23e+3;
    char c = 'c';
    int i = 9;
    float *p = &f;

    // Constant variables
    const char k = 'k';
    // Constant pointers, basic pointer operations: address of (&var), dereference (*var)
    const char *const pk = &k;
    // should print the character 'k'
    printf("%c\n", *pk);

    /*
      Multi-line comment
      return -1;
    */

    // Single-line comment

    return 0;
}
