typedef unsigned long long    uint64_t;
typedef unsigned              uint32_t;

uint32_t mul32(uint32_t a, uint32_t b)
{
  return a*b;
}

uint64_t mul64(uint32_t a, uint32_t b)
{
  // only cast first operand
  return (uint64_t) a*b;
}
