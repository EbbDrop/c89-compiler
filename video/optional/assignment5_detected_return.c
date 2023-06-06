// * Assignment 5:
//   - Check that all execution paths end in a return statement

int simple() {
  return 1;
}

int with_if(int i) {
  if (i) {
    return 1;
  } else {
    return 0;
  }
  // no return needed here
}

int inf_loop_also_counts(int i) {
  if (i) {
    while (1) {

    }
    // no return needed here since loop is infinite
  } else {
    return 0;
  }
}

int with_switch(int i) {
  switch (i) {
    case 1:
    case 2:
      return 0;
    case 3:
    case 4:
      return 1;
    default:
      return 2;
  }
}

int main() { return 0; }
