
volatile int i;

int main(int argc, char** argv) {

    int c = 0;

    i = 0;

    while (i < 2) {
	c++;
    }

    return c;
}
