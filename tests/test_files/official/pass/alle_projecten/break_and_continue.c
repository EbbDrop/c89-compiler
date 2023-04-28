//output:
//0
//1
//2
//3
//4
//5

int main(){
	int i = 0;
	while(i < 10){
		printf(i);
		if (i == 5){
			break;
		} else {
			i++;
			continue;
		}
		i = 10;
	}
}
