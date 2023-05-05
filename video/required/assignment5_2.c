// • Assignment 5:
//   – Functions:
//     ∗ Definition and declaration
//     ∗ Calling
//     ∗ Parameters (primitives and pointers, pass-by-value, pass-by-reference)
//     ∗ Return values
//     ∗ Functions with void return
//   – Type checking for return statements

#include <stdio.h>

// forward declare fp_add
float fp_add(float, float b); // giving names to the params is optional

int int_add(float a, float b) {
  if (a == b) {
    return (int)(2.0 * a);
  }
  return (int)fp_add(a, b);
}

float fp_add(float a, float b) { return a + b; }

// Not all params need to be named
int return_first(int a, int, float) { return a; }

// Pass a pointer to update the pointed-to value, to simulate pass-by-reference
void increment_pointee(int *p) {
  (*p)++;
  // function returns void
  return;
}

// float* type_check_return(float p) {
//     return p; // return a float, while the function's return value is float*
// }

int main() {
  printf("int_add(2.0 + 2.0) = %i\n", int_add(2.0, 2.0));
  printf("fp_add(2.0 + 2.0) = %f\n", fp_add(2.0, 2.0));
  printf("return_first(1, 2, 3.0) = %i\n", return_first(1, 2, 3.0));

  int a = 2;
  int *p = &a;
  increment_pointee(p);
  printf("int a = 2; int *p = &a; increment_pointee(p);\n => a: %i", a);

  return 0;
}
