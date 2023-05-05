// Extra feature: Bitwise shifts (type checking)

int main() {
  int int_ = 3;
  float float_ = 3.2;
  int* pointer = &int_;


  int_ >> pointer;
  pointer << int_;
  pointer << pointer;
  float_ >> int_;
  float_ >> float_;

  return 0;
}
