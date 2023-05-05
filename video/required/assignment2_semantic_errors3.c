// Assignment 2:
// – Semantic errors + error message:
//   ∗ Assignment to an rvalue expression
//   ∗ Re-assignment of const variables

void assignment_to_rvalue_expr() {
    (234 + 9) = 8;
    int a = 2;
    int* p = &a;
    (*p | 2) = 8;
}

void reassignment_of_const_vars() {
    const int a = 3;
    a = 4;
}

int main() {
    assignment_to_rvalue_expr();
    reassignment_of_const_vars();
    return 0;
}
