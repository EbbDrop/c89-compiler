//output:
//0
//1
//2
//3
//4
//5
//-1
//6
//-1
//7
//-1
//8
//-1
//9
//-1
//10

int main() {
    int boolean = 1;
    int a = 0;
    while (boolean) {
        printf(a++);
        if (a > 10) {
            break;
        } else if (a < 6) {
            continue;
        }
        printf(-1);
    }
}
