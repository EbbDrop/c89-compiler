//fail:
//NeedLvalue
//NeedLvalue
//NeedLvalue

int main() {
    int a = 3;
    (a + 2) = 4;
    2 = 3;
    ((int)a) = 3;
}
