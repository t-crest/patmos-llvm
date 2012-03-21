#include <stdio.h>

void foo();

static void baz() {
    foo();
}

void bar() {
//    foo();
}

void foo() {
    bar();
}

void test1() {
  printf("hello world 1\n");
}

int main(int argc, char** argv) {
  test1();
  if (argc) {
      foo();
  } else {
      bar();
  }
  return 0;
}

