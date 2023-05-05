// Assignment 2:
// – Semantic errors + error message:
//   ∗ Operations and assignments with incompatible types

int main() {
    int i = 2;
    float f = 3.8;
    char c = 'c';
    int *p = i;
    // operations
    p + p;
    i - p;
    f % i;
    p < f;
    c & f;
    p | i;
    // assignments
    p = f;
    p = c;
    i = p;
    return 0;
}
