#include "common.h"
int f(int a);
int g_pre(int a);
int g(int a);
int h(int a, int b);

DEFINE_WORK(5)
DEFINE_WORK(7)
DEFINE_WORK(11)
DEFINE_WORK(13)

__attribute__((noinline))
int f(int a)
{
  if(a) {
    return g_pre(0) + g_pre(1);
  } else {
    return g_pre(0) + g_pre(0);
  }
}
__attribute__((noinline))
int g_pre(int a)
{
  return g(a);
}
__attribute__((noinline))
int g(int a)
{
  return h(a,0) + h(a,1);
}
__attribute__((noinline))
int h(int a, int b)
{
  if(a && b) {
    return work_13();
  } else if(a) {
    return work_11();
  } else if(b) {
    return work_7();
  } else {
    return work_5();
  }
}
#define EXPECTED_RESULT 7227
int main() {
  int s1 = f(0);
  int s2 = f(1);
  OUT(printf("Result 0: %d\n", s1));
  OUT(printf("Result 1: %d\n", s2));
  int r = s1 + s2;
  OUT(printf("Result Sum: %d\n", r));
  EXIT_MISMATCH(r,EXPECTED_RESULT);
  return 0;
}

