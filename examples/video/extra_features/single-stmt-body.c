// Extra feature: Single statement body

int ifs(int i) {
  if (i)
    return i - 1;
  else
    i = 8;

  if (i == 1) {
    // do thing
  } else if (i == 2) {
    // do other thing
  } else if (i == 3) {
    // and another
  } else if (i == 4) {
    // you should probably have used a switch by now :)
  } else {
    // we got there
  }

  if (i)
    return 1;
  else {
    return i;
  }
}

void whiles(int i) {
  while (i)
    i++;

}

void fors(int i) {
  // yes this is suported by C89
  for(;i;i++) ;
}

int main() { return 0; }
