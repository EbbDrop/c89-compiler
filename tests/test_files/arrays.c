//output:
//0
//1
//2
//3
//0
//1
//2
//3

int main() {
  int a[4];
  **&a = 0;
  a[1] = 1;
  2[a] = 2;
  a[3] = 3;
  printf(a[0]);
  printf(a[1]);
  printf(a[2]);
  printf(a[3]);

  int b[2][2];
  b[0][0] = 0;
  b[0][1] = 1;
  b[1][0] = 2;
  b[1][1] = 3;
  printf(b[0][0]);
  printf(b[0][1]);
  printf(b[1][0]);
  printf(b[1][1]);

  int c[4];
  int* d = a;
}
