extern int _test;

void test(int i) {
    asm("add $r2 = $r1, %0\n\t"
	"mov $r10 = %1" :  : "r" (i), "r" (_test));
}
