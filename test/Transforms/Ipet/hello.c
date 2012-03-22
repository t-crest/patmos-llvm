#include <stdio.h>

void foo();

void foo2();

static void baz() {
    foo();
}

void bar() {
    foo2();
    foo();
}

void foo() {
    bar();
    baz();
}

void test1() {
  printf("hello world 1\n");
}

void tricky(int a) {
    void (*f)(void);
    f = a ? foo : bar;
    f();
}

int main(int argc, char** argv) {
  test1();
  if (argc) {
      foo();
  } else {
      bar();
  }
  tricky(argc - 1);
  bar();
  return 0;
}

