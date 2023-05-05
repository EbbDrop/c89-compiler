//fail:
//IncompatibleReturn
//ValueReturnInVoid
//NoReturnValue

int a() {
  int a = 3;
  return &a;
}

void b() {
  return 5;
}

int c() {
  return ;
}
