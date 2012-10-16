#include <stdio.h>

int main() {
    double volatile x[3] = { 2.0, 6.0, 2.5 };
    int cnt = 2;

    for (int i = 0; i < cnt; i++) {
	printf(" %f", x[i]);
    }

    printf("\n");
    return 0;
}
