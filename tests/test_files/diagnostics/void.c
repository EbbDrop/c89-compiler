//fail:
//VoidVariable
//VoidVariable
//VoidVariable
//VoidUsed
//NeedLvalue
//UnexpectedType
//UnexpectedType
//VoidUsed
//UnexpectedType

void c(int c) {
  
}

void no_void_param(void v) {}

void v;

int main() {
  void a;
  int b = (void)4;
  c(1)++;
  c(1) + 3; 
  *c(1);
  c(c(1));
  c(1) * 3; 
}
