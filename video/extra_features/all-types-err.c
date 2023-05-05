// Extra feature: Support for all c types (errors)

int main() {
  // can't have short and double
  short double a;

  // floats can't have specified signedness
  signed float b;
  unsigned float c;
  signed short float j;

  // chars can't have size specifier
  short char d;
  long char e;

  // long and short contradict eachother
  long short int f;
  // unsigned and signed contadict eachother
  unsigned signed int g;
  unsigned int signed i;

  // can't have two base types
  int float h;
}
