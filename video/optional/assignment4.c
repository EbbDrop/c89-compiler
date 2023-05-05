// * Assignment 4:
//   - switch statements with case, break, and default

// doubles the numbers between 1 and 3. Returning -1 otherwise
#include <stdio.h>

int simple(int i) {
  switch (i) {
    case 0:
      return 0;
    case 1:
      return 2;
    case 2:
      return 4;
    case 3:
      return 6;
    default:
      return -1;
  }
}

// gives 1 for even number between 0 and 10, and 0 otherwise
int fallthrough(int i) {
  switch (i) {
    case 0:
    case 2:
    case 4:
    case 6:
    case 8:
    case 10:
      return 1;
    default:
      return 0;

  }
}

// same as above but uses a default in the center becouse we can
int center_default(int i) {
  switch (i) {
    case 0:
    case 2:
    case 1:
      return 1;

    default:
    case 3:
      return 0;

    case 4:
    case 6:
    case 8:
    case 10:
      return 1;
  }
}

int main() {
  printf("%i\n", simple(0)); // expected 0
  printf("%i\n", simple(1)); // expected 2
  printf("%i\n", simple(2)); // expected 4
  printf("%i\n", simple(7)); // expected -1
  printf("\n");

  printf("%i\n", fallthrough(0)); // expected 1
  printf("%i\n", fallthrough(6)); // expected 1
  printf("%i\n", fallthrough(3)); // expected 0
  printf("%i\n", fallthrough(7)); // expected 0
  printf("\n");

  printf("%i\n", center_default(0)); // expected 1
  printf("%i\n", center_default(6)); // expected 1
  printf("%i\n", center_default(3)); // expected 0
  printf("%i\n", center_default(7)); // expected 0
  return 0;
}
