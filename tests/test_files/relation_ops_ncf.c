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

int one = 1;
int* op = &one;
int o = *op;

int three = 3;
int* tp = &three;
int t = *tp;


printf(o > t);
printf(t > o);
printf(t > t);
printf(o < t);
printf(t < o);
printf(t < t);
printf(o <= t);
printf(t <= t);
printf(t <= o);
printf(o >= t);
printf(t >= t);
printf(t >= o);
printf(o == o);
printf(2 == 4);
printf(2 != t);
printf(t != t);
