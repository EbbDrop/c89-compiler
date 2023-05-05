// • Assignment 5:
//   – Type checking for forward declarations

// declaration of f
int f(int a, int b);

// declaration of f again, but with different parameter names (allowed)
int f(int c, int d);

// definition of f (allowed)
int f(int a, int b) {
    return a*b;
}

// declaration of f again (allowed)
int f(int, int);

// definition of f again (not allowed)
int f(int a, int b) {
    return a*b;
}

// definition of f again, but with different parameter names (not allowed)
int f(int c, int d) {
    return c*d;
}

// definition of f again, but with different body (not allowed)
int f(int a, int b) {
    int r = a*b;
    return r;
}

// declaration of f again, but with different amount of parameters (not allowed)
int f(int, int, int);

// declaration of f again, but with different param types (not allowed)
int f(float a, int b);

// declaration of f again, but with different return type
float f(int a, int b);

int main() {
    g(); // calling undeclared function
    return 0;
}
