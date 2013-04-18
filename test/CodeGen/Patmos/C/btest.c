
extern int x;

void btest1(int flags, int b)
{
  if ( flags & (1 << b) ) x = 123;
}
void btest1a(int flags, int b)
{
  if (!( flags & (1 << b))) x = 123;
}

void btest2(int flags)
{
  if ( (flags & 16) == 0 ) x = 123;
}
void btest2a(int flags)
{
  if (!((flags & 16) == 0)) x = 123;
}

void  btest3(int flags)
{
  if ( (flags & 16) == 16 ) x = 123;
}
void  btest3a(int flags)
{
  if (!((flags & 16) == 16)) x = 123;
}
