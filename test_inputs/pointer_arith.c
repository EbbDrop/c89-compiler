int i = 4;
int* ptri = &i;
printf(ptri);
// moves pointer 4 places (int is 4 bytes)
printf(ptri + 1);

char c = 'a';
char* ptrc = &c;
printf(ptrc);
// moves pointer one place (char is one byte)
printf(ptrc + 1);
