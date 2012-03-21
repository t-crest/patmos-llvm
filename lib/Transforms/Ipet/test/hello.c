#include <stdio.h>

extern int foo();

void test1() {
  int a = foo();
  printf("hello world %d\n", a);
}

int main() {
  test1();
  return 0;
}

