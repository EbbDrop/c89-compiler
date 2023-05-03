//fail:
//IncompatibleAssign
//UnexpectedType
//IncompatibleAssign

int main() {
    int a[4];
    a = 4;
    a++;
    *(&a) = 4;
}
