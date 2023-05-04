//warn:
//NotAlwaysReturn
//NotAlwaysReturn
//NotAlwaysReturn
//NotAlwaysReturn

int f(int f) {
  if (f) {
  } else {
    return 1;
  }
}

int a(int f) {
  if (f) {
    for (;;) { }
  } else {
  }
}

int b(int f) {
  for (;f;) {
    return 1;
  }
}

int c(int f) {
  switch (f) {
    case 1:
      return 1;
    case 2:
      for (;;) { }
    default:
      (void)f;
  }
}

int main() {
  f(1);
  a(1);
  b(2);
  c(4);
  return 0;
}