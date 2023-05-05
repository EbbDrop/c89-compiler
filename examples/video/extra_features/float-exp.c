// Extra feature: Full float syntax

#include <stdio.h>

int main() {
  printf("1.     = %f\n", 1.   );
  printf(".1     = %f\n", .1   );
  printf("1e2    = %f\n", 1e2  );
  printf("1e-2   = %f\n", 1e-2 );
  printf("1E2    = %f\n", 1E2  );
  printf("1E-2   = %f\n", 1E-2 );
  printf("1.e2   = %f\n", 1.e2 );
  printf(".1e2   = %f\n", .1e2 );
  printf("1.e-2  = %f\n", 1.e-2);
  printf(".1e-2  = %f\n", .1e-2);
  printf("1.E2   = %f\n", 1.E2 );
  printf(".1E2   = %f\n", .1E2 );
  printf("1.E-2  = %f\n", 1.E-2);
  printf(".1E-2  = %f\n", .1E-2);
}

