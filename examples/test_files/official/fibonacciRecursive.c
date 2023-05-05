//output:
//Enter a number:
//fib(2)	= 1;
//fib(3)	= 2;
//fib(4)	= 3;
//fib(5)	= 5;
//fib(6)	= 8;
//fib(7)	= 13;
//fib(8)	= 21;
//fib(9)	= 34;
//fib(10)	= 55;
//fib(11)	= 89;

#include <stdio.h>

int f(int a) {
	if (a<2) {
		return a;
	}
	else {
		return f(a-1) + f(a-2);
	}
}

// Recursive fibonnaci
int main(){
	int n;
  printf("Enter a number:\n");
	//scanf("%d",&n);
	n = 10;
	int i = 1;
	while(i++ <= n){
		printf("fib(%d)\t= %d;\n", i, f(i));
	}
	return 0;
}
