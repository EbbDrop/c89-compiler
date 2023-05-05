//fail:
//ClashingGlobalName
//IncompatibleFunctionRedef
//IncompatibleFunctionRedef
//IncompatibleFunctionRedef
//MultipleFunctionDef
//ClashingGlobalName
//IncompatibleVariableRedef
//IncompatibleVariableRedef
//IncompatibleVariableRedef
//MultipleVariableDef

int a = 4;

int a();

int b(int a);

float b(int a);

int b(int a, int c);

int b(float a);

int c() {
  return 1;
}

int c() {
  return 0;
}

int c = 2;

int d;
float d;

const int e;
int e;

int f;
const int f;

int g = 2;
int g = 3;
