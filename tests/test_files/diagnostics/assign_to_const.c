//fail:
//NeedConst
//IncompatibleAssign
//NeedConst

int main() {
    const int a = 3;
    a = 2;
    int* ptr = &a;
    const int* ptr2 = &a;
    *ptr2 = 54;
}
