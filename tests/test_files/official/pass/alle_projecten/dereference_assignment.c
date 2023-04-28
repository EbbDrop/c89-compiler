//output:
//10
//10
//11
//11

int main(){
	int x = 0;
	int* xp = &x;
	*xp = 10;
	printf(x);
	printf(*xp);
	(*xp)++;
	printf(x);
	printf(*xp);
}
