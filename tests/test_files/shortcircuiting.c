//output:
//3
//0
//2

int main() {
    int a = 3;
    printf(a);

    (a = 0) && (a = 255);

    printf(a);

    (a = 2) || (a = 255);

    printf(a);
}
