/** @file debias.c
 *
 * Implementation of a von-neumann debiassing of a random bitstream.
 */

#include "debias.h"

int
transfer_bits_and_debias(BitBuffer from, BitBuffer to)
{
  BitField bits;
  int bits_xfered = 0;
  while (bitbuffer_available_bits(from) >= 2) {
    if (bitbuffer_extract_bits(from, &bits, 2) != 2)
      return bits_xfered;
    if ((bits & 3) == 1 || (bits & 3) == 2) {
      if (bitbuffer_add_bits(to, bits, 1) != 1)
        return bits_xfered;
      bits_xfered++;
    }
  }
  return bits_xfered;
}
