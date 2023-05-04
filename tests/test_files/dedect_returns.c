//warn:

int f(int f) {
  if (f) {
    for (;;) {

    }
  } else {
    return 1;
  }
}

int a(int f) {
  if (f) {
    for (;;) { }
  } else {
    for (;;) { }
  }
}

int b(int f) {
  for (;1;) {
    return 1;
  }
}

int c(int f) {
  switch (f) {
    case 1:
    case 2:
      for (;;) { }
    case 3:
      if (f) {
        return 1;
      } else {
        return 1;
      }
    default:
      return 1;
  }
}

int d(int f) {
  while (f) {
    return 1;
  }
  return 1;
}

int main() {
  f(1);
  a(1);
  b(2);
  c(4);
  d(4);
  return 0;
}