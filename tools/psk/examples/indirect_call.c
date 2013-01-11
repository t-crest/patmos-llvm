#include <stdint.h>

/* from: http://en.wikipedia.org/wiki/Random_number_generation */
volatile uint32_t m_w = 241;
volatile uint32_t m_z = 9923;
uint32_t nondet() {
    m_z = 36969 * (m_z & 65535) + (m_z >> 16);
    m_w = 18000 * (m_w & 65535) + (m_w >> 16);
    return (m_z << 16) + m_w;  /* 32-bit result */
}

#define FNS 3
#define ITER 8
#define MAX 7

typedef uint32_t (*ftype)(uint32_t);

uint32_t f1(uint32_t in) { return in % 6; }
uint32_t f2(uint32_t in) { return in % 4; }
uint32_t f3(uint32_t in) { return in % 7; }

ftype funs[FNS] = {f1,f2,f3};

int main(int argc, char** argv) {
  int i;
  uint32_t r;

  for(i = 0; i < ITER; i++)
    r += funs[nondet() % FNS](nondet());

  if(r > ITER*MAX) return 1;

  return 0;
}
