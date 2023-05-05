// * Assignment 5:
//   - No code is generated for if-clauses that are never true

int always_true() {
  int a;
  if (1) {
    // This is kept
    a = 0;
  } else {
    // This is removed
    a = 1;
  }
  return a;
}

int always_false() {
  int a;
  if (0) {
    // This is removed
    a = 1;
  } else {
    // This is kept
    a = 0;
  }
  return a;
}
