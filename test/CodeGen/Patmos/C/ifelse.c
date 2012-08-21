
int ifelse(int a, int b)
{
  int c;
  if (a > b) {
    c = a-b;
  } else if (a < b) {
    c = b-a;
  } else {
    c = a;
  }
  return c;
}
