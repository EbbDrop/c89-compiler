void func() {}

int main() {
  int a = 1;
  int b = 2;
  int c = 3;
  int d = 4;
  int e = 5;
  int f = 6;
  int g = 7;
  int h = 8;
  int i = 9;
  int j = 10;
  int k = 11;
  // Call to make shure ONLY saved registers can be used,
  // limiting the amout of usable registers to 7
  func();
  return a + b + c + d + e + f + g + h + i + j + k;
}
