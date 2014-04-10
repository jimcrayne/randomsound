/** @file debias.h
 *
 * Perform a von-neumann debiassing of a random bitstream.
 */

#ifndef RANDOMSOUND_DEBIAS_H
#define RANDOMSOUND_DEBIAS_H

#include "bitbuffer.h"

/** Transfer bits from one buffer to another, debiassing en-route.
 *
 * Performing a von-neumann debiassing, transfer bits from the \a from
 * ::BitBuffer to the \a to ::BitBuffer.
 *
 * This method of debiassing will produce at absolute maximum, a drop
 * of 50% in the bitcount. I.E. at minimum you will lose one bit in two.
 *
 * In actuality, in a truly random stream, you will lose three bits in
 * four, which means that the debiassing will leave you with around
 * one quarter the number of bits you started with.
 *
 * @param from The ::BitBuffer to extract bits from.
 * @param to The ::BitBuffer to add debiassed bits to.
 * @return The number of bits added to \a to.
 */
int transfer_bits_and_debias(BitBuffer from, BitBuffer to);

#endif
