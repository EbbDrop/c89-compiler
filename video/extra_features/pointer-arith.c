
#include <stdio.h>

int main() {
    int i = 4;
    int* ptri = &i;
    // moves pointer 4 places (int is 4 bytes)
    int* ptri2 = ptri + 1;
    printf("%i\n", (int)ptri2 - (int)ptri);
    // subtracting raw pointers divides by the size of the pointer, so should be 1
    printf("%i\n", ptri2 - ptri);

    char c = 'a';
    char* ptrc = &c;
    // moves pointer one place (char is one byte)
    char* ptrc2 = ptrc + 1;
    printf("%i\n", (int)ptrc2 - (int)ptrc);

    // subtracting raw pointers divides by the size of the pointer, so should be 1
    printf("%i\n", ptrc2 - ptrc);

    int j = 0;
    int* p = &j;
    int** ptrp = &p;
    // moves pointer 4/8 place depending on having a 32 or 64 bit proccessor
    int** ptrp2 = ptrp + 1;
    printf("%i\n", (int)ptrp2 - (int)ptrp);

    // subtracting raw pointers divides by the size of the pointer.
    // so should be 1 independent of proccessor
    printf("%i\n", ptrp2 - ptrp);

    return 0;
}
