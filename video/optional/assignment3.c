// • Assignment 3:
//   – Store comments in AST and machine code
//   – Comment machine instructions with the original C statement

// Global singe line comment here
int aa = 4;

/*
Multi line comment here
- a line
- more lines
- 42
*/
int bb = 4;

// multiple types
/* of comments */
// Are combined
int main() {
  int i = 4;

  // Comment for the if
  if (i == 2) {
    // comment in the if body
    i + 2;
  } else {
    // comment in the else body
    i + 2;
  }

  return 0;
}