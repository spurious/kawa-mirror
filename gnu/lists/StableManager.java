// Copyright (c) 2001, 2002, 2003, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** Implements a stable sequence with sticky positions.
 * I.e if you have a position, it gets automatically updated after
 * insertions and deletions. */

public class StableManager
{
  SimpleVector base;
  /** This array maps from the exported ipos values (indexes in the positions
   * array) to the ipos of the underlying SimpleVector base.
   * The first two elements are reserved for START_POSITION and END_POSITION.
   * Unused elements in positions are chained together in a free list
   * headed by the 'free' variable.  */
  protected int[] positions;

  /** The head of the free elements in position, if they are chained.
   * We keep track of available elements in the positions array in two ways:
   * In unchained mode, there is no free list per se.  Instead an index i
   * is available if positions[i]==FREE_POSITION.  This mode makes it
   * easy to loop over all positions, ignoring the unused ones.
   * In chained mode, there is a free list and if index i is available,
   * then positions[i] is the next available index, with -1 if there is none.
   * Unchained mode is indicated by free==-2.
   * In chained mode, free is the first element in the free list,
   * or -1 if the free list is empty.
   * The main virtue of this convention is that we don't need a separate
   * list or array for the free list.  But we should get rid of the
   * unchained mode, at least.  FIXME.
   */
  protected int free;

  /** An invalid value for an in-use element of positions. */
  protected static final int FREE_POSITION = -1 << 1;

  /** Put all free elements in positions in a chain starting with free. */
  protected void chainFreelist()
  {
    free = -1;
    for (int i = positions.length;  --i > END_POSITION; )
      {
	int pos = positions[i];
	if (pos == FREE_POSITION)
	  {
	    positions[i] = free;
	    free = i;
	  }
      }
  }

  /** Set all free elements in positions to FREE_POSITION. */
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

  public int startPos () { return START_POSITION; }
  public int endPos () { return END_POSITION; }

  public StableManager(SimpleVector base)
  {
    this.base = base;
    base.gapReserveGeneric(base.size(), 0); // To force to GAP mode.
    positions = new int[16];
    positions[START_POSITION] = 0;
    positions[END_POSITION] = (base.getBufferLength() << 1) | 1;
    free = -1;
    for (int i = positions.length;  --i > END_POSITION; )
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

  public int createPos (int index, boolean isAfter)
  {
    if (index == 0 && ! isAfter)
      return START_POSITION;
    else if (isAfter && index == base.size())
      return END_POSITION;
    int gapStart = base.getGapStart();
    int gapEnd = base.getGapEnd();
    if (index > gapStart || (index == gapStart && isAfter))
      index += gapEnd - gapStart;
    int ipos = allocPositionIndex();
    positions[ipos] = (index << 1) | (isAfter ? 1 : 0);
    return ipos;
  }

  protected boolean isAfterPos (int ipos)
  {
    return (positions[ipos] & 1) != 0;
  }

  public boolean hasNext(int ipos)
  {
    int ppos = positions[ipos];
    int index = ppos >>> 1;
    int gapStart = base.getGapStart();
    if (index >= gapStart)
        index += base.getGapEnd() - gapStart;
    return index < base.getBufferLength();
  }

  public int nextPos (int ipos)
  {
    int ppos = positions[ipos];
    int index = ppos >>> 1;
    int gapStart = base.getGapStart();
    int gapEnd = base.getGapEnd();
    if (index >= gapStart)
      index += gapEnd - gapStart;
    if (index >= base.getBufferLength())
      {
	releasePos(ipos);
	return 0;
      }
    if (ipos == 0)
      ipos = createPos(0, true);
    positions[ipos] = ppos | 1;
    return ipos;
  }

  public int nextIndex (int ipos)
  {
    int index = positions[ipos] >>> 1;
    int gapStart = base.getGapStart();
    int gapEnd = base.getGapEnd();
    if (index > gapStart)
      index -= gapEnd - gapStart;
    return index;
  }

  public void releasePos(int ipos)
  {
    if (ipos >= 2)
      {
	if (free == -2)
	  chainFreelist();
	positions[ipos] = free;
	free = ipos;
      }
  }

  public int copyPos (int ipos)
  {
    if (ipos > END_POSITION)
      {
	int i = allocPositionIndex();
	positions[i] = positions[ipos];
	ipos = i;
      }
    return ipos;
  }

    /** Adjust gap to 'where', and make sure it is least `needed'
     * elements long. */
    protected void gapReserve(SimpleVector base, int where, int needed) {
        int oldGapEnd = base.getGapEnd();
        int oldGapStart = base.getGapStart();
        int oldLength = base.getBufferLength();
        base.gapReserveGeneric(where, needed);
        if (needed > oldGapEnd - oldGapStart) {
            int newLength = base.getBufferLength();
            if (where == oldGapStart) // Optimization.
                adjustPositions(oldGapEnd << 1, (newLength << 1) | 1,
                                (newLength - oldLength) << 1);
            else {
                // We do adjustPositions twice which is wasteful but simple.
                // Adjust positions as if there were no gap.
                adjustPositions(oldGapEnd << 1, (oldLength << 1) | 1, (oldGapStart - oldGapEnd) << 1);
                // Adjust positions for new gap.
                int gapStart = base.getGapStart();
                int gapEnd = base.getGapEnd();
                adjustPositions(gapStart << 1, (newLength << 1) | 1, (gapEnd - gapStart) << 1);
            }
        } else if (where != oldGapStart) {
            int delta = where - oldGapStart;
            int low, high, adjust;
            if (delta > 0) {
                low = oldGapEnd;
                high = low + delta;
                adjust = (oldGapStart - low) << 1;
                // The position corresponding to the new endGap should be
                // adjusted only if it has the isAfter (low-order) bit is clear.
                low = low << 1;
                high = high << 1;
            }
            else { // newGapStart < oldGapStart:
                // Positions at the newgapEnd should be adjust only if isAfter.
                low = (where << 1) + 1;
                high = (oldGapStart << 1) + 1;
                adjust = (oldGapEnd - oldGapStart) << 1;
            }
            adjustPositions(low, high, adjust);
        }
    }

  /** Add a delta to all positions elements that point into a given range.
   * Assume {@code x==positions[i]}, then if
   * {@code (unsigned)x>=(unsigned)low && (unsigned)x <= (unsigned)high},
   * then add {@code delta} to {@code positions[i]}.
   * Using unsigned comparisons allows us to compare ipos values,
   * which include both the index and the isAfter low-order bit.   */
  protected void adjustPositions(int low, int high, int delta)
  {
    // Swing has positions in an array ordered by offset.
    // That means it can use binary search to find only those
    // positions that need to adjust, rather than check all the positions
    // (including the 'free' ones).  FIXME.

    if (free >= -1)
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

    /*
  protected int addPos(int ipos, E value)
  {
    int ppos = positions[ipos];
    int index = ppos >>> 1;
    if (index >= gapStart)
      index += gapEnd - gapStart;
    // Force positions[ipos] to have the isAfter property.
    if ((ppos & 1) == 0)
      {
	if (ipos == 0)
	  ipos = createPos(0, true);
	else
	  positions[ipos] = ppos | 1;
      }
    add(index, value);
    return ipos;
  }
    */

  protected void removePosRange(int ipos0, int ipos1)
  {
      throw new Error();
      /*
      super.removePosRange(positions[ipos0], positions[ipos1]);

    // adjust positions in gap
    int low = gapStart;
    int high = gapEnd;
    if (free >= -1)
      unchainFreelist();
    for (int i = positions.length;  --i > START_POSITION; )
      {
	int pos = positions[i];
	if (pos != FREE_POSITION)
	  {
	    int index = pos >> 1;
            boolean isAfter = (pos & 1) != 0;
            if (isAfter)
              {
                if (index >= low && index < high)
                  positions[i] = (gapEnd << 1) | 1;
              }
            else
              {
                if (index > low && index <= high)
                  positions[i] = (gapStart << 1);
              }
	  }
      }
      */
  }

  /*
  public Object remove(int index)
  {
    // FIXME
  }
  */

  public void consumePosRange (int iposStart, int iposEnd, Consumer out)
  {
      throw new Error();
      //super.consumePosRange(positions[iposStart], positions[iposEnd], out);
  }

  /* DEBUGGING
  void checkInvariants()
  {
      boolean wasChained = free >= -1;
      if (wasChained)
           unchainFreelist();
      int gapStart2 = base.getGapStart() << 1;
      int gapEnd2 = base.getGapEnd() << 1;
      for (int i = positions.length; --i >= 0; )  {
          int pos = positions[i];
          if (pos == FREE_POSITION)
              continue;
          if (pos < 0 || nextIndex(i) > base.size()
              || (pos > gapStart2 && pos <= gapEnd2)) {
              throw new Error("bad position#"+i+": "+pos+" gapStart:"+base.getGapStart()+" gapEnd:"+base.getGapEnd()+" bufLen:"+base.getBufferLength());
          }
      }
      if (wasChained)
          chainFreelist();
  }
  */
}
