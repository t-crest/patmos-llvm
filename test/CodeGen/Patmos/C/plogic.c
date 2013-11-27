void foo(int a, int b, int c)
{
  volatile int x, y;
  if (! ((a<b) && (c==13)) || (b>=c) ) x = 34;
}
