// • Assignment 4:
//   – if and else statements
//   – while loops
//   – for loops
//   – break statements
//   – continue statements
//   – Anonymous scopes
//   – Symbol table for anonymous scopes

#include <stdio.h>

void if_else(int a) {
    if (a < 2) {
        printf("%i < 2\n", a);
    } else if (a > 2) {
        printf("%i > 4\n", a);
    } else {
        printf("%i == 2\n", a);
    }
}

/*
 Function that should print:
  counting down: 2
  counting down: 1
  counting down: 0
  0
  2
  4
*/
void loops() {
    int a = 3;
    while (a --> 0) {
        printf("counting down: %i\n", a);
    }

    for (int x = 0; x < 5; x = x + 2) {
        printf("%i\n", x);
    }
}

/*
 Function that should print:
   1
   3
   5
   b: 0
   one
   b: 3
*/
void break_and_continue() {
    int always_true = 10;
    int i = 1;
    while (always_true) {
        if (i == 2 || i == 4) { ++i; continue; }
        if (i % 2 == 0) break;
        printf("%i\n", i);
        ++i;
    }
    for (int b = 0;;) {
        switch (b) {
        case 0:
        case 3:
            break;
        default:
            ++b;
            continue;
        case 1:
            printf("one\n");
            b = 2;
            continue;
        }
        printf("b: %i\n", b++);
        if (b > 3) {
            break;
        }
    }
}

// Function that should print
//  0
//    1
//      2
//      3
//    1
//  0
void anonymous_scope() {
    int a = 0;
    printf("%i\n", a);
    {
        int a = 1;
        printf("  %i\n", a);
        {
            int a = 2;
            printf("    %i\n", a);
            a++;
            printf("    %i\n", a);
        }
        printf("  %i\n", a);
    }
    printf("%i\n", a);
}

int main() {
    if_else(2);
    loops();
    break_and_continue();
    anonymous_scope();
    return 0;
}
