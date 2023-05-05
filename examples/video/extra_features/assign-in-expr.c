// Extra feature: Assing in expresions

#include <stdio.h>

int main() {
  int a = 0;

  printf("(a)         => %i\n", a);
  printf("(a = 2)     => %i\n", a = 2);
  printf("(a)         => %i\n", a);
  printf("3 + (a = 2) => %i\n", 3 + (a = 3));
  printf("(a)         => %i\n", a);

  return 0;
}
