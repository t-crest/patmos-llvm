#include <stdbool.h>
bool test(int a);

int check(int a) {
  bool b = test(a);
  return b ? 10 : 20;
}

