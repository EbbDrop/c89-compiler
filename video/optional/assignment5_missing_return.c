// * Assignment 5:
//   - Check that all execution paths end in a return statement

int forgot_return() {
}

int only_one_path_return(int i) {
  if (i) {
    return 1;
  } else {
    // no return here
  }
}

int break_blocks_return(int i) {
  for (;;) {
    break;
    return 1; // does not count becouse of break
  }
}

int main() { return 0; }
