
int cmov(int a, int c, int b)
{
  int x = a+1;
  return (c) ? x : b;
}

int cli(int a, int c)
{
  int x = a+1;
  return (c) ? x : 100;
}

int cnor(int a, int b, int c, int d)
{
  int res = (c) ? a : ~( b | d );
  return res;
}

int cand(int a, int b, int c)
{
  int res = (c) ? a : ( b & (1<<15) );
  return res;
}

int csr(int a, int b, int c)
{
  int res = (c) ? a :  (b >> 13) ;
  return res;
}

int mul_cli(int a, int b, int c)
{
  return (c>b) * 123;
}

