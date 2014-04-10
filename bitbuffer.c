/** @file bitbuffer.h
 *
 * Implementation of a circular buffer of bits.
 *
 * @author Daniel Silverstone
 */
#include <stdlib.h>

#include "bitbuffer.h"

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

struct bitbuffer_s {
  int size;
  int firstused;
  int nextfree;
  int bitsused;
  BitField bits[0];
};

#define BYTE_FOR_BIT(n) (n>>3)
#define SHIFT_FOR_BIT(n) (n&7)

BitBuffer
bitbuffer_new(const int size)
{
  int bytesize = ((size + 7) & ~7) >> 3;
  BitBuffer ret = (BitBuffer)(malloc(sizeof(struct bitbuffer_s) + bytesize));
  ret->size = size;
  ret->firstused = 0;
  ret->nextfree = 0;
  ret->bitsused = 0;
  return ret;
}

void
bitbuffer_free(BitBuffer buf)
{
  free(buf);
}

int
bitbuffer_free_space(const BitBuffer buf)
{
  return buf->size - buf->bitsused;
}

int
bitbuffer_available_bits(const BitBuffer buf)
{
  return buf->bitsused;
}

int
bitbuffer_add_bits(BitBuffer buf, const BitField bits, int count)
{
  int bits_to_add = MIN(MIN(8, count), bitbuffer_free_space(buf));
  int i;
  for (i = 0; i < bits_to_add; ++i) {
    BitField subfield = buf->bits[BYTE_FOR_BIT(buf->nextfree)];
    if (bits & (1<<SHIFT_FOR_BIT(i)))
      subfield |= (1<<SHIFT_FOR_BIT(buf->nextfree));
    else
      subfield &= ~(1<<SHIFT_FOR_BIT(buf->nextfree));
    buf->bits[BYTE_FOR_BIT(buf->nextfree++)] = subfield;
    if (buf->nextfree == buf->size)
      buf->nextfree = 0;
  }
  buf->bitsused += bits_to_add;
  return bits_to_add;
}

int
bitbuffer_extract_bits(BitBuffer buf, BitField *bits, int count)
{
  int bits_to_extract = MIN(MIN(8, count), bitbuffer_available_bits(buf));
  int i;
  for (i = 0; i < bits_to_extract; ++i) {
    BitField subfield = buf->bits[BYTE_FOR_BIT(buf->firstused)];
    if (subfield & (1<<SHIFT_FOR_BIT(buf->firstused++)))
      *bits |= (1<<SHIFT_FOR_BIT(i));
    else
      *bits &= ~(1<<SHIFT_FOR_BIT(i));
    if (buf->firstused == buf->size)
      buf->firstused = 0;
  }
  buf->bitsused -= bits_to_extract;
  return bits_to_extract;
}


#ifdef TEST

#include <stdio.h>

void
xassert(const char* msg, int val, int line, BitBuffer buf, BitField bits)
{
  if (val == 0) {
    fprintf(stdout, "FAILURE on line %d of condition %s\n", line, msg);
    fprintf(stdout, "Buffer at %p: size %d firstused %d nextfree %d\n",
            buf, buf->size, buf->firstused, buf->nextfree);
    fprintf(stdout, "Bitfield is %02x\n", bits);
    exit(1);
  }
}

#ifdef assert
#undef assert
#endif

#define assert(X) xassert(#X, X, __LINE__, buf, bits)

int main(int argc, char** argv)
{
  BitBuffer buf;
  BitField bits = 0xA5;
  
  buf = bitbuffer_new(32);
  assert(buf != NULL);
  assert(bitbuffer_free_space(buf) == 32);
  assert(bitbuffer_available_bits(buf) == 0);
  
  assert(bitbuffer_add_bits(buf, bits, 8) == 8);
  assert(bitbuffer_add_bits(buf, bits, 8) == 8);
  assert(bitbuffer_add_bits(buf, bits, 8) == 8);
  assert(bitbuffer_add_bits(buf, bits, 8) == 8);
  
  assert(bitbuffer_add_bits(buf, bits, 8) == 0);
  
  assert(bitbuffer_free_space(buf) == 0);
  assert(bitbuffer_available_bits(buf) == 32);
  
  assert(bitbuffer_extract_bits(buf, &bits, 8) == 8);
  assert(bits == 0xA5);
  assert(bitbuffer_extract_bits(buf, &bits, 8) == 8);
  assert(bits == 0xA5);
  assert(bitbuffer_extract_bits(buf, &bits, 8) == 8);
  assert(bits == 0xA5);
  assert(bitbuffer_extract_bits(buf, &bits, 8) == 8);
  assert(bits == 0xA5);
  
  assert(bitbuffer_extract_bits(buf, &bits, 8) == 0);
  
  bits = 0x96;
  assert(bitbuffer_add_bits(buf, bits, 8) == 8);
  bits = 0xF;
  assert(bitbuffer_add_bits(buf, bits, 4) == 4);
  bits = 0x96;
  assert(bitbuffer_add_bits(buf, bits, 8) == 8);
  bits = 0xF;
  assert(bitbuffer_add_bits(buf, bits, 4) == 4);
  
  assert(bitbuffer_extract_bits(buf, &bits, 8) == 8);
  assert(bits == 0x96);
  assert(bitbuffer_extract_bits(buf, &bits, 8) == 8);
  assert(bits == 0x6F);
  assert(bitbuffer_extract_bits(buf, &bits, 4) == 4);
  assert((bits & 0xf) == 0x9);
  assert(bitbuffer_extract_bits(buf, &bits, 8) == 4);
  assert((bits & 0xf) == 0xF);
  
  return 0;
}
#endif
