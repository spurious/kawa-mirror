// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** Implements a stable sequence with sticky positions.
 * I.e if you have a position, it gets automatically updated after
 * insertions and deletions. */

public class StableVector extends GapVector
{
  /** This array maps from the exported ipos values (indexes in the positions
   * array to the ipos of the underlying SimpleVector base.
   * The first two elements are reserved for START_POSITION and END_POSITION.
   * Unused elements in positions are chained together in a free list
   * headed by the 'free' variable.  */
  protected int[] positions;

  /** The index of the first free element in positions, or -1 if none.
   * If free == -2, there is is no free element chain, but free elements
   * have the value FREE_POSITION. */
  protected int free;

  /** An invalid value for an in-use element of positions. */
  protected static final int FREE_POSITION = -1 << 1;

  /** Put all free elements in positions in a chain starting with free. */
  protected void chainFreelist()
  {
    free = -1;
    for (int i = positions.length;  --i >= END_POSITION; )
      {
	int pos = positions[i];
	if (pos == FREE_POSITION)
	  {
	    positions[i] = pos;
	    free = i;
	  }
      }
  }

  /** At all free elements in positions to FREE_POSITION. */
  protected void unchainFreelist()
  {
    for (int i = free;  i >= 0; )
      {
	int next = positions[i];
	positions[i] = FREE_POSITION;
	i = next;
      }
    free = -2;
  }

  /** Index in positions for the start position.
   * positions[START_POSITION] is always 0. */
  static final int START_POSITION = 0;

  /** Index in positions for the end position.
   * positions[END] is always (size()<<1)+1. */
  static final int END_POSITION = 1;

  public StableVector(SimpleVector base)
  {
    super(base);
    positions = new int[16];
    positions[START_POSITION] = 0;
    positions[END_POSITION] = (base.getBufferLength() << 1) | 1;
    free = -1;
    for (int i = positions.length;  --i >= 2; )
      {
	positions[i] = free;
	free = i;
      }
  }

  protected int allocPositionIndex()
  {
    if (free == -2)
      chainFreelist();
    if (free < 0)
      {
	int oldLength = positions.length;
	int[] tmp = new int[2 * oldLength];
	System.arraycopy(positions, 0, tmp, 0, oldLength);
	for (int i = 2 * oldLength;  --i >= oldLength; )
	  {
	    tmp[i] = free;
	    free = i;
	  }
	positions = tmp;
      }
    int pos = free;
    free = positions[free];
    return pos;
  }

  protected void makeStartPosition(PositionContainer posSet, int posNumber)
  {
    posSet.setPosition(posNumber, START_POSITION, null);
  }

  protected void makeEndPosition(PositionContainer posSet, int posNumber)
  {
    posSet.setPosition(posNumber, END_POSITION, null);
  }

  public int createPosition(int index, boolean isAfter)
  {
    if (index == 0 && ! isAfter)
      return START_POSITION;
    else if (isAfter && index == size())
      return END_POSITION;
    if (index > gapStart || (index == gapStart && isAfter))
      index += gapEnd - gapStart;
    int ipos = allocPositionIndex();
    positions[ipos] = (index << 1) | (isAfter ? 1 : 0);
    return ipos;
  }

  protected void makePosition(int index, boolean isAfter,
			      PositionContainer posSet, int posNumber)
  {
    int ipos = createPosition(index, isAfter);
    posSet.setPosition(posNumber, ipos, null);
    posSet.setSequence(posNumber, this); // FIXME - handled by caller?
  }

  protected boolean isAfter(int ipos)
  {
    return (positions[ipos] & 1) != 0;
  }

  protected boolean hasNext(int ipos, Object xpos)
  {
    int ppos = positions[ipos];
    int index = ppos >>> 1;
    if (index >= gapStart)
      index += gapEnd - gapStart;
    return index < base.getBufferLength();
  }

  protected boolean gotoNext(PositionContainer posSet, int posNumber)
  {
    int ipos = posSet.getPositionInt(posNumber);
    int ppos = positions[ipos];
    int index = ppos >>> 1;
    if (index >= gapStart)
      index += gapEnd - gapStart;
    if (index >= base.getBufferLength())
      return false;
    if (ipos == 0)
      ipos = createPosition(0, true);
    positions[ipos] = ppos | 1;
    return true;
  }

  public int nextIndex(int ipos, Object xpos)
  {
    int index = positions[ipos] >>> 1;
    if (index > gapStart)
      index -= gapEnd - gapStart;
    return index;
  }

  public void releasePosition(int ipos, Object xpos)
  {
    if (ipos >= 2)
      {
	if (free == -2)
	  chainFreelist();
	positions[ipos] = free;
	free = ipos;
      }
  }

  public void copyPosition(int ipos, Object xpos,
			   PositionContainer posSet, int posNumber)
  {
    if (ipos > END_POSITION)
      {
	int i = allocPositionIndex();
	positions[i] = positions[ipos];
	ipos = i;
      }
    posSet.setPosition(posNumber, ipos, null);
    posSet.setSequence(posNumber, this); // FIXME - handled by caller?
  }

  protected void shiftGap(int newGapStart)
  {
    int oldGapStart = gapStart;
    int delta = newGapStart - oldGapStart;
    int low, high, adjust;
    if (delta > 0)
      {
	low = gapEnd;
	high = low + delta;
	adjust = (oldGapStart - low) << 1;
	// The position corresponding to the new endGap should be adjusted
	// only if it has the isAfter (low-order) bit is clear.  Hence the -1.
	low = low << 1;
	high = (high << 1) - 1;
      }
    else if (newGapStart == oldGapStart)
      return;
    else // newGapStart < gapStart:
      {
	// Positions at the newgapStart should be adjust only if isAfter.
	low = (newGapStart << 1) + 1;
	high = oldGapStart << 1;
	adjust = (gapEnd - oldGapStart) << 1;
      }
    super.shiftGap(newGapStart);

    adjustPositions(low, high, adjust);
  }

  /** Add a delta to all positions elements that point into a given range.
   * Assume x==positions[i], then if (unsigned)x>=(unsigned)low
   * && (unsigned)x <= (unsigned)high, then add delta to positions[i].
   * Using unsigned comparisons allows us to compare ipos values,
   * which include both the index and the isAfter low-order bit.   */
  protected void adjustPositions(int low, int high, int delta)
  {
    if (free >= 0)
      unchainFreelist();

    // Invert the high-order bit, because:
    // (unsigned) X > (unsigned) Y
    // iff (int) (X^0x80000000) > (int) (Y^0x80000000)
    low = low ^ 0x80000000;
    high = high ^ 0x80000000;

    for (int i = positions.length;  --i > START_POSITION; )
      {
	int pos = positions[i];
	if (pos != FREE_POSITION)
	  {
	    int index = pos ^ 0x80000000;
	    if (index >= low && index <= high)
	      positions[i] = pos + delta;
	  }
      }
  }

  protected void gapReserve(int size)
  {
    int oldGapEnd = gapEnd;
    int oldLength = base.getBufferLength();
    super.gapReserve(size);
    int newLength = base.getBufferLength();
    adjustPositions(oldGapEnd << 1, (newLength << 1) | 1,
		    (newLength - oldLength) << 1);
  }

  protected void add(PositionContainer posSet, int posNumber, Object value)
  {
    int ipos = posSet.getPositionInt(posNumber);
    int ppos = positions[ipos];
    int index = ppos >>> 1;
    if (index >= gapStart)
      index += gapEnd - gapStart;
    // Force positions[ipos] to have the isAfter property.
    if ((ppos & 1) == 0)
      {
	if (ipos == 0)
	  ipos = createPosition(0, true);
	else
	  positions[ipos] = ppos | 1;
      }
    add(index, value);
  }

  protected void remove(int ipos0, Object xpos0, int ipos1, Object xpos1)
  {
    super.remove(positions[ipos0], null, positions[ipos1], null);

    // adjust positions in gap
    int low = gapStart;
    int high = gapEnd;
    for (int i = positions.length;  --i >= START_POSITION; )
      {
	int pos = positions[i];
	if (pos != FREE_POSITION)
	  {
	    int index = pos >> 1;
	    if (index >= low && index <= high)
	      positions[i]
		= (pos & 1) != 1 ? (gapEnd << 1) | 1 : (gapStart << 1);
	  }
      }
  }

  /*
  public Object remove(int index)
  {
    // FIXME
  }
  */

  public void consume(int iposStart, Object xposStart,
		      int iposEnd, Object xposEnd, Consumer out)
  {
    super.consume(positions[iposStart], null, positions[iposEnd], null, out);
  }
}
