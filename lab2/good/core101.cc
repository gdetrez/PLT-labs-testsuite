bool foo(int x) {
  bool result ;
  if (x == 0) {
    result = false;
  } else {
    bool b = true;
    
    printInt(x);
    result = b;
  }
  return result;
}

void printBool(bool b) {
  if (false) {}
  else
    if (b) {
      printInt(0);
    } else {
      printInt(1);
    }
}

int main() {
  printBool(foo(42));
  printBool(foo(0));
  return 0;
}
