#include <stdlib.h>
#include <stdio.h>

int main() {
    printf("Shift: %d\n", -1 >> 1);
    abort();

    printf("Unreachable!\n");
    return 0;
}
