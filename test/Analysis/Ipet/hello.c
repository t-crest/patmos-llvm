#include <stdio.h>

void foo();

void foo2();

static void baz() {
    foo2();
}

void bar() {
    foo2();
}

void test1() {
  printf("hello world 1\n");
}

void foo(int b) {
    if (b == 0) {
	bar();
    } else if (b == 1) {
	baz();
    } else {
	int c = b + 2;
	if (c == 4) {
	    test1();
	}
    }
}

void tricky(int a) {
    void (*f)(void);
    f = a ? foo2 : bar;
    f();
}

int main(int argc, char** argv) {
  test1();
  if (argc) {
      foo(argc);
  } else {
      bar();
  }
  tricky(argc - 1);
  bar();
  return 0;
}

