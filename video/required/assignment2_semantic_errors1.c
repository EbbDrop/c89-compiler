// Assignment 2:
// – Semantic errors + error message:
//   ∗ Usage of uninitialised and undeclared variables
//   ∗ Redeclarations and redefinitions of existing variables

int global = 0;

int usage_of_uninitialized_variable(int a) {
    int b;
    int c = 9;
    return a + b; // Usage of the uninitialized variable `b`.
}

int usage_of_undeclared_variable() {
    global = a; // Usage of the undeclared variable `a`.
    int a = 3;
    return a;
}

void redeclaration_and_redefinition_of_existing_variable(float p) {
    int a; // First declaration of `a`.
    int a; // Redeclaration of `a`.
    int a = 3; // Redefinition of `a`.
    int* p; // Redeclaration of function param with different type.
    const float p = 0.3; // Redefinition of function param with different constness.
}

int main() {
    usage_of_uninitialized_variable(1);
    usage_of_undeclared_variable();
    redeclaration_and_redefinition_of_existing_variable(2.8);
    return 0;
}

int var;
// definition with different type
float var = 4;
int var = 2;
// redefinition of `var`
int var = 2;
