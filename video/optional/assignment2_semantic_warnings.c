// • Assignment 2:
//   – Type conversions (implicit and explicit)

#include <stdio.h>

int main() {
  // implicit conversions
  int i = 3.4;
  char c = 1234;

  // explicit to supress warnings
  int i_no_err = (int)3.4;
  char c_no_err = (char)1234;

  // int gets converted to float
  printf("%f\n", 3 + 3.4);

  // bet here the float becomes a int
  printf("%i\n", 3 + (int)3.4);

  return 0;
}
