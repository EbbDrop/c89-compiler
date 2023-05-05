// Assignment 1:
//  - Binary arithmetic operators: +, -, *, /
//  – Binary comparison operators: <, >, ==
//  – Unary operators: +var, -var
//  – Parentheses, order of operation
//  – Logical operators: &&, ||, !var
//  – Ignore whitespace in code
//  – Constant folding
//  – Constructing and using the AST

#include <stdio.h>

int main() {
    int a = 3;
    int b = 4;

    // To prevent const propagation to cause const folding on the next line,
    // a printf is inserted.
    printf("a = %i; b = %i;\n", a, b);

    // Binary arithmetic operators: +, -, *, /
    // should be 3
    int c = ((a + b) - 1) / 2;
    printf(
        "\nbinary arithmetic operators:\n"
        "((a + b) - 1) / 2:  %i\n", c);

    // Binary comparison operators: <, >, ==
    printf(
        "\nbinary comparison operators:\n"
        "a = %i; b = %i; c = %i;\n"
        "a == c:  %i\n"   // should be 1
        "b > a :  %i\n"   // should be 1
        "b < a :  %i\n",  // should be 0
        a, b, c, a == c, b > a, b < a);

    // Unary operators: +var, -var
    printf(
        "\nunary operators:\n"
        "a = %i;\n"
        "+(-a):  %i\n"     // should be -3
        "-(-a):  %i\n"     // should be 3
        "- -1 :  %i\n",     // should be 1
        a, +(-a), -(-a), - -1);

    // Logical operators
    printf(
        "\nlogical operators:\n"
        "a = %i; b = %i;\n"
        "(a == a) && (b == b):  %i\n"  // should be 1
        "(a != a) || (b != b):  %i\n"  // should be 1
        "!(a == a)           :  %i\n", // should be 0
        a, b,
        (a == a) && (b == b),
        (a != a) || (b != b),
        !(a == a));

    // Constant folding
    printf(
        "\nconst folding:\n"
        "-1 * (9 - ((5 + 5) / 2)):  %i\n", // should be -4 (as a constant in generated llvm)
        -1 * (9 - ((5 + 5) / 2)));

    return 0;
}
