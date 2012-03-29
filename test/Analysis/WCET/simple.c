

int main()
{
  int sum, a = 1;
  if (1 == a) {
    return 0;
  } else {
    int i;
    sum = 0;
    for (i=0; i<10; i++) {
      sum += i;
      for (int j = i; j<5; j++) {
	  sum += j;
      }
    }
  }
  return sum;
}
