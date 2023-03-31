int a = 4;
int *ptr = &a;
int **ptrptr = &ptr;
**ptrptr = 8;
printf(a);
