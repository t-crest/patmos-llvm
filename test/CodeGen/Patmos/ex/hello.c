#include <stdio.h>

void test(const char *s, int i) {
    printf("%s : %d\n", s, i);
}

int main(int argc, char** argv) {
    const char *s;

    if (argc < 2) {
	s = "Hello World!";
    } else {
	s = "Goodbye";
    }

    test(s, argc);

    return 0;
}
