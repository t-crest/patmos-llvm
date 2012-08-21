
int forloop(int a, int b)
{
  int i,c;
  for (i=0,c=0; i<a; i++) {
    c += b ^ i;
  }
  return c;
}
