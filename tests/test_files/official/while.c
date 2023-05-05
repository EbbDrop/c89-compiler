//output:
//1;2;3;4;5;

#include <stdio.h>

// Should print the numbers 1 - 5
int main(){
	int i = 0;
	while (i < 5){
		i++;
		printf("%d;", i);
	}
	return 0;
}
