// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/**
 * An array with a gap in the middle, allowing efficient insert and delete.
 * The actual storage is delegated to a SimpleVector, so the element
 * type depends on the sub-class of SimpleVector used.
 */

public class GapVector extends AbstractSequence implements Sequence
{
  public SimpleVector base;
  public int gapStart;
  public int gapEnd;

  public GapVector(SimpleVector base)
  {
    this.base = base;
  }

  public int size()
  {
    return base.getBufferLength() - (gapEnd - gapStart);
  }

  public boolean hasNext(int ipos)
  {
    int index = ipos >>> 1;
    if (index >= gapStart)
      index += gapEnd - gapStart;
    return index < base.getBufferLength();
  }

  public int getNextKind(int ipos)
  {
    return hasNext(ipos) ? base.getElementKind() : EOF_VALUE;
  }

  public Object get (int index)
  {
    // If index is out of bounds, the base.get will catch that.
    if (index >= gapStart)
      index += gapEnd - gapStart;
    return base.get(index);
  }

  public Object set (int index, Object value)
  {
    // If index is out of bounds, the base.set will catch that.
    if (index >= gapStart)
      index += gapEnd - gapStart;
    return base.set(index, value);
  }

  public void fill (Object value)
  {
    base.fill(gapEnd, base.getBufferLength(), value);
    base.fill(0, gapStart, value);
  }

  public void fillPosRange(int fromPos, int toPos, Object value)
  {
    int from = fromPos == -1 ? base.size : fromPos >>> 1;
    int to = toPos == -1 ? base.size : toPos >>> 1;
    int limit = gapStart < to ? gapStart : to;
    for (int i = from;  i < limit;  i++)
      base.setBuffer(i, value);
    for (int i = gapEnd;  i < to;  i++)
      base.setBuffer(i, value);
  }

  protected void shiftGap(int newGapStart)
  {
    int delta = newGapStart - gapStart;
    if (delta > 0)
      base.shift(gapEnd, gapStart, delta);
    else if (delta < 0)
      base.shift(newGapStart, gapEnd + delta, - delta);
    gapEnd += delta;
    gapStart = newGapStart;
  }

  /** Make sure gap is at least 'size' elements long. */
  protected void gapReserve(int size)
  {
    if (size > gapEnd - gapStart)
      {
	int oldLength = base.getBufferLength();
        int newLength = oldLength < 16 ? 16 : 2 * oldLength;
	int minLength = oldLength - (gapEnd - gapStart) + size;
	if (newLength < minLength)
	  newLength = minLength;
	// FIXME  this does unneeded copying.
	// It may also leave unwanted junk in the gap (gap for gc).
	base.setBufferLength(newLength);
	int newGapEnd = newLength - oldLength + gapEnd;
	base.shift(gapEnd, newGapEnd, oldLength - gapEnd);
	gapEnd = newGapEnd;
      }
  }

  /** Adjust gap to 'where', and make sure it is least `size' elements long. */
  protected void gapReserve(int where, int size)
  {
    gapReserve(size);
    if (where != gapStart)
      shiftGap(where);
  }

  protected int addPos (int ipos, Object value)
  {
    int index = ipos >>> 1;
    if (index >= gapStart)
      index += gapEnd - gapStart;
    add(index, value);
    return ((index + 1) << 1) | 1;
  }

  public void add(int index, Object o)
  {
    gapReserve(index, 1);
    base.set(index, o);
    gapStart++;
  }

  protected void removePosRange(int ipos0, int ipos1)
  {
    ipos0 >>>= 1;
    ipos1 >>>= 1;
    if (ipos0 > gapEnd)
      shiftGap(ipos0);
    else if (ipos1 < gapStart)
      shiftGap(ipos1);
    if (ipos0 < gapStart)
      {
	base.clearBuffer(ipos0, gapStart - ipos0);
	gapStart = ipos0;
      }
    if (ipos1 > gapEnd)
      {
	base.clearBuffer(gapEnd, ipos1 - gapEnd);
	gapEnd = ipos1;
      }
  }

  /* FIXME - ths is quite wrong!
  public Object remove(int index)
  {
    if (index >= gapStart)
      index += gapEnd - gapStart;
    return base.remove(index);
  }
  */

  public int createPos(int index, boolean isAfter)
  {
    if (index > gapStart)
      index += gapEnd - gapStart;
    // if (index == gapStart && isAfter) index = gapEnd; ??
    return (index << 1) | (isAfter ? 1 : 0);
  }

  protected boolean isAfterPos(int ipos)
  {
    return (ipos & 1) != 0;
  }

  protected int nextIndex(int ipos)
  {
    int index = ipos == -1 ? base.getBufferLength() : ipos >>> 1;
    if (index > gapStart)
      index -= gapEnd - gapStart;
    return index;
  }

  public void consumePosRange (int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    if (i < gapStart)
      {
	int lim = end > gapStart ? end : gapStart;
	consumePosRange(iposStart, lim << 1, out);
      }
    if (end > gapEnd)
      {
	i = i < gapEnd ? gapEnd : i;
	consumePosRange(i << 1, iposEnd, out);
      }
  }

}
