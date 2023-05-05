// Extra feature:  short circuiting logical `and` and `or`

#include <stdio.h>

int print_a_true() {
  printf("a");
  return 1;
}

int print_b_true() {
  printf("b");
  return 1;
}

int print_a_false() {
  printf("a");
  return 0;
}

int print_b_false() {
  printf("b");
  return 0;
}

int main() {
  //and:
  // both print
  print_a_true() && print_b_true();
  printf("\n");

  // b is never printed
  print_a_false() && print_b_true();
  printf("\n\n");


  //or:
  // b is never printed
  print_a_true() || print_b_true();
  printf("\n");

  // both print
  print_a_false() || print_b_true();
  printf("\n");

  return 0;
}
