/* from stefan */
#include <stdio.h>

__attribute__((noinline))
int test(int arg) {
    int i = 20 - arg;
    int j = -arg;
    int k = 1;

    /* loop here max arg ; */
    for (k = 0; k < arg; k++) {
	if ( i % 2 ) i /= 2;
	else i = i*3 + 1;
    }

    switch (arg+4) {
	case 0: i = 40; j = 40; break;
	case 1: i = 20; j = 10; break;
	case 2: i = 30; j = 40; break;
	case 3: i = 10; break;
	case 4: i = 64; j = 15; break;
	case 5: i = 50; break;
	case 6: i = 40; break;
    }
    return i+j;
}

int main(int argc, char** argv) {
  int r = 0;
  int i;
  /* loop here exactly 20 ; */
  for(i = 0; i < 20; i++) {
    r = test(i - 10) ^ r;
  }
  if(r != -124) return 1;
  return 0;
}
