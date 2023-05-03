//warn:
//LossyImplicitAssign
//LossyImplicitAssign

int main() {
    int a = 4;
    char b = a;
    float c = 4.32;
    int d = c;
    int e = (int)c; // No warning with explicit cast
}
