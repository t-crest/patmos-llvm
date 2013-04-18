
volatile int x;

void isodd(int a)
{
  if ((a&1)==1) {
    x = a + 3;
  } else {
    x = -x;
  }
}
