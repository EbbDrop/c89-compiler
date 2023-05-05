// • Assignment 5:
//   – No dead code after return, break, and continue

void foo(int a) {
    if (a > 2) {
        // part of the generated llvm
        ++a;
        return;
        // not part of the generated llvm (dead code)
        ++a;
    }

    for (int b = a;; ++b) {
        if (b > 8) {
            // part of the generated llvm
            break;
            // not part of the generated llvm
            --b;
        }
        continue;
        // not part of the generated llvm
        --a;
    }
}

int main() {
    foo(1);
    return 0;
}
