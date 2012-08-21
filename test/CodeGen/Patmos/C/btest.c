
int btest1(int flags, int b)
{
  return flags & (1 << b);
}

int btest2(int flags, int b)
{
  return (flags & b) == b;
}

int btest3(int flags)
{
  return (flags & 16) == 16;
}

