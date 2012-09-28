
extern int var;


int second(int a)
{
  return -a;
}


int first(int a, int b)
{
  return second(a+b);
}


int main(int argc, char **argv)
{
  first(var, 3);
  return 0;
}
