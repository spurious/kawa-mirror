// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** An object that contains some number of positions in a Sequence.
 * Each position is a pair of an int and an Object.  These have meaning
 * only as interpreted by the Sequence.
 *
 * This interface is purely for efficiency - rather than allocating
 * a fresh SeqPosition object for each pair, we can store more than
 * one pair in a container.
 */

public interface PositionContainer
{
  /** Get the integer part of a specified position pair. */
  public int getPositionInt(int positionNumber);

  /** Get the Object part of a specified position pair. */
  public Object getPositionPtr(int positionNumber);

  public void setPosition(int positionNumber, int ipos, Object xpos);
  public void setSequence(int positionNumber, AbstractSequence seq);

  /** Get the number of positions pairs stored in this PositionContainer. */
  public int countPositions();
}
