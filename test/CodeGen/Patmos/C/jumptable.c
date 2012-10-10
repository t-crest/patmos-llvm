
int main(int argc, char**argv) {
    int i = 0;
    int j = 1;
    switch(argc) {
	case 1: i = 40; j = 50; break;
	case 2: i = 10; j = 60; break;
	case 3: i = 12; j = 13; break;
	case 4: i = 15; j = 10; break;
	case 5: i = 34; j = 12; break;
	case 6: i = 90; j = 80; break;
    }
    return i+j;
}
