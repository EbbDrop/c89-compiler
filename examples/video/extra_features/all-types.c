// Extra feature: Support for all c types

int main() {
  float a;
  double b;
  char c = 'c';
  signed char d;
  short int e;
  unsigned long int f;
  signed long int g = 3;
  long int h = 2; 


  // warn on loss of sign
  f = g;

  // warn of loss of presision
  e = h;

  // c gets converted into long int before add
  h + c;

  return 0;
}
