//output:
//4
//4
//8

int main() {
    int a = 4;
    printf(a);
    int* ptr = &a;
    int** ptrptr = &ptr;
    printf(a);
    **ptrptr = 8;
    printf(a);
}
