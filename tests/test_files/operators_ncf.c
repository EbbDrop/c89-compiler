//ignore
//output:
//8
//2
//15
//1
//3
//-3
//2
//5
//9.000000
//1.400000
//19.760000
//1.368421
//3.800000
//-3.800000

// TODO: stop ignoring once varargs with floats are fixed

#include <stdio.h>

int main() {
    int five = 5;
    int* fp = &five;
    int f = *fp;

    int three = 3;
    int* tp = &three;
    int t = *tp;

    float three_eight = 3.8;
    float* tep = &three_eight;
    float te = *tep;

    printf("%i\n", f + t);
    printf("%i\n", f - t);
    printf("%i\n", f * t);
    printf("%i\n", f / t);
    printf("%i\n", + t);
    printf("%i\n", - t);
    printf("%i\n", f % t);
    printf("%i\n", f % 9);
    printf("%f\n", 5.2 + te);
    printf("%f\n", 5.2 - te);
    printf("%f\n", 5.2 * te);
    printf("%f\n", 5.2 / te);
    printf("%f\n", + te);
    printf("%f\n", - te);
}
