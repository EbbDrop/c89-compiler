//output:
//3
//-1
//255

int main() {
    int a = 3;
    int b;
    switch (a) {
    case 3:
        printf(3);
    default:
        printf(-1);
        break;
    case 6:
        printf(6);
        break;
    case 9:
        printf(9);
    }
    printf(255);
}
