//output:
//0
//1
//2
//3
//4
//5

int main() {
    // array of 3 element where every element is a array of two ints
    int arr[3][2];
    arr[0][0] = 0;
    arr[0][1] = 1;
    arr[1][0] = 2;
    arr[1][1] = 3;
    arr[2][0] = 4;
    arr[2][1] = 5;
    int* ptr = (int*)arr;
    printf(*(ptr++));
    printf(*(ptr++));
    printf(*(ptr++));
    printf(*(ptr++));
    printf(*(ptr++));
    printf(*(ptr++));
}