// • Assignment 1:
//   – Additional logical operators: >=, <=, !=
//   – Modulo operator: %
// • Assignment 2:
//   – Increment, decrement operators: ++, -- (both prefix and suﬀix variants)

#include <stdio.h>

int main() {
  printf(
    "(-1 >=  4) = %i\n"    // expect 0
    "( 4 >=  4) = %i\n"    // expect 1
    "( 4 >= -1) = %i\n\n", // expect 1
    -1 >=  4,
     4 >=  4,
     4 >= -1
  );

  printf(
    "5 %% 3 = %i\n"    // expect 2
    "4 %% 5 = %i\n\n", // expect 4
    5 % 3,
    4 % 5
  );

  int a = 0;
  printf("  a = %i\n", a);    // expect 0
  printf("++a = %i\n", ++a);  // expect 1
  printf("  a = %i\n", a);    // expect 1
  printf("--a = %i\n", --a);  // expect 0
  printf("  a = %i\n", a);    // expect 0
  printf("a++ = %i\n", a++);  // expect 0
  printf("  a = %i\n", a);    // expect 1
  printf("a-- = %i\n", a--);  // expect 1
  printf("  a = %i\n\n", a);  // expect 0

  return 0;
}
