//output:
//20
//30
//40

int main(){
	int x = 20;
	printf(x);
    x = 30;
	if (1){
		printf(x);
		int x = 40;
		printf(x);
	}
}
