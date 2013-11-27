/*
 * Conditionally set or clear bits without branching
 *
 * Taken from Bit Twiddling Hacks:
 * http://graphics.stanford.edu/~seander/bithacks.html#ConditionalSetOrClearBitsWithoutBranching
 */

/**
 * bool f;         // conditional flag
 * unsigned int m; // the bit mask
 * unsigned int w; // the word to modify:  if (f) w |= m; else w &= ~m;
 */
unsigned int bitsetclr0(int f, unsigned int m, unsigned int w)
{
  if (f) w |= m; else w &= ~m;
  return w;
}

unsigned int bitsetclr1(int f, unsigned int m, unsigned int w)
{
  if ( f) w |=  m;
  if (!f) w &= ~m;
  return w;
}

unsigned int bitsetclr2(int f, unsigned int m, unsigned int w)
{
  w ^= (-f ^ w) & m;
  return w;
}

// OR, for superscalar CPUs:
unsigned int bitsetclr3(int f, unsigned int m, unsigned int w)
{
  w = (w & ~m) | (-f & m);
  return w;
}
