//output:
//0
//1
//0
//1
//0
//0
//1
//1
//0
//0
//1
//1
//1
//0
//1
//0

#include <stdio.h>

int main() {
    float one = 1.0;
    float* op = &one;
    float o = *op;

    float three = 3.0;
    float* tp = &three;
    float t = *tp;


    printf("%i\n", o > t);
    printf("%i\n", t > o);
    printf("%i\n", t > t);
    printf("%i\n", o < t);
    printf("%i\n", t < o);
    printf("%i\n", t < t);
    printf("%i\n", o <= t);
    printf("%i\n", t <= t);
    printf("%i\n", t <= o);
    printf("%i\n", o >= t);
    printf("%i\n", t >= t);
    printf("%i\n", t >= o);
    printf("%i\n", o == o);
    printf("%i\n", 2 == 4);
    printf("%i\n", 2 != t);
    printf("%i\n", t != t);
}
