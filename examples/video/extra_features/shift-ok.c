// Extra feature: Bitwise shifts

#include <stdio.h>

int main() {
  printf(
    "7  << 1 = %i\n"
    "1  << 3 = %i\n"
    "2  << 2 = %i\n"
    "48 >> 1 = %i\n"
    "20 >> 2 = %i\n"
    "4  >> 3 = %i\n",
    7  << 1,
    1  << 3,
    2  << 2,
    48 >> 1,
    20 >> 2,
    4  >> 5
  );

  return 0;
}
