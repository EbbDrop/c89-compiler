//output:
//4
//1

#include <stdio.h>

int main() {
    int i = 4;
    int* ptri = &i;
    // moves pointer 4 places (int is 4 bytes)
    int* ptri2 = ptri + 1;
    printf("%i\n", (int)ptri2 - (int)ptri);

    char c = 'a';
    char* ptrc = &c;
    // moves pointer one place (char is one byte)
    char* ptrc2 = ptrc + 1;
    printf("%i\n", (int)ptrc2 - (int)ptrc);
}
