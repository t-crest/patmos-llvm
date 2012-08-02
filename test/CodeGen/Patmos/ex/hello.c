#include <stdio.h>

void test(const char *s, int i) {
    int j = i / 3;
    float f = 0.5f;
    float k = f / 2.0f;
    printf("%s : %d %f\n", s, j, k);
}

int main(int argc, char** argv) {
    const char *s;

    if (argc < 2) {
	s = "Hello World!";
    } else {
	s = "Goodbye";
    }

    test(s, 8 + argc);

    return 0;
}
