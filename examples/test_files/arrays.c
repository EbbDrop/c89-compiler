//output:
//0
//1
//2
//3
//0
//1
//2
//3

#include <stdio.h>

int main() {
  int a[4];
  **&a = 0;
  a[1] = 1;
  2[a] = 2;
  a[3] = 3;
  printf("%i\n", a[0]);
  printf("%i\n", a[1]);
  printf("%i\n", a[2]);
  printf("%i\n", a[3]);

  int b[2][2];
  b[0][0] = 0;
  b[0][1] = 1;
  b[1][0] = 2;
  b[1][1] = 3;
  printf("%i\n", b[0][0]);
  printf("%i\n", b[0][1]);
  printf("%i\n", b[1][0]);
  printf("%i\n", b[1][1]);

  int c[4];
  int* d = a;
}
