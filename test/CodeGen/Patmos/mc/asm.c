extern int _test;

int test(int i) {
    int k, l;
    asm("add $r2 = $r1, %2\n\t"
	"add $r10 = %2, %3\n\t"
	"li $r3 = 0x1234ABCD\n\t"
	"mov %0 = $r31"
	: "=r" (k)
	: "r" (l), "0" (i), "{r20}" (_test));
    return k;
}
