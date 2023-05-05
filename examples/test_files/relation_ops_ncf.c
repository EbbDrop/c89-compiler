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
    int one = 1;
    int* op = &one;
    int o = *op;

    int three = 3;
    int* tp = &three;
    int t = *tp;


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
