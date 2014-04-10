/** @file bitbuffer.h
 *
 * A circular buffer of bits.
 * (with an additional method for filling the 
 *  buffer with microphone data.)
 *
 * @author Daniel Silverstone
 */

#ifndef RANDOMSOUND_BITBUFFER_H
#define RANDOMSOUND_BITBUFFER_H

/** A bit buffer.
 *
 * A circular buffer of a given number of bits.
 */
typedef struct bitbuffer_s *BitBuffer;
/** A bit field, cunningly disguised as an unsigned char. */
typedef unsigned char BitField;

/** Allocate a new bitbuffer.
 *
 * Given the \a size requested, create a new bit buffer which can store
 * that number of bits and return it having initialised it properly.
 *
 * @param size The number of bits to store in this buffer.
 * @return The new ::BitBuffer
 */
BitBuffer bitbuffer_new(const int size);

/** Fill a bit buffer with sound bits.
 *
 * Fill a bitbuffer with bits using the microphone
 *
 * @param buf The ::BitBuffer to fill.
 */
int bitbuffer_fill(BitBuffer buf);

/** Free a bit buffer.
 *
 * Free a bitbuffer and all associated storage.
 *
 * @param buf The ::BitBuffer to free.
 */
void bitbuffer_free(BitBuffer buf);

/** Get free space in bit buffer.
 *    
 * Return the number of free bits in \a buf.
 *
 * @param buf The ::BitBuffer in question.
 * @return The number of free bits in the buffer.
 */
int bitbuffer_free_space(const BitBuffer buf);

/** Get the number of bits available for extraction.
 *
 * Return the number of bits stored in the buffer and which are thus
 * available for extraction.
 *
 * @param buf The ::BitBuffer in question
 * @return The number of bits available for extraction.
 */
int bitbuffer_available_bits(const BitBuffer buf);

/** Store some bits into a buffer.
 *
 * Store \a count of bits out of \a bits into \s buf.
 *
 * This routine works least-significant-bit first so if you are only
 * storing one bit, make sure it's in bit zero of the ::BitField.
 *
 * @param buf The ::BitBuffer to add to.
 * @param bits The ::BitField to find the bits in.
 * @param count The number of bits to add to \a buf.
 * @return The number of bits actually added.
 */
int bitbuffer_add_bits(BitBuffer buf, const BitField bits, int count);

/** Extract some bits from a buffer.
 *
 * Extract \a count of bits out of \a buf into \s bits.
 *
 * This routine works least-significant-bit first so if you are only
 * extracting one bit, it will be in bit zero of the ::BitField.
 *
 * @param buf The ::BitBuffer to extract from.
 * @param bits A pointer to a ::BitField to put the bits in.
 * @param count The number of bits to add to \a bits.
 * @return The number of bits actually added.
 */
int bitbuffer_extract_bits(BitBuffer buf, BitField *bits, int count);

#endif
