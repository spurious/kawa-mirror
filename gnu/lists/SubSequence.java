// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A sequence consisting of a sub-range of the elements of a base sequence.
 * The start and end positions are positions triples (on the same sequence).
 */

public class SubSequence
extends AbstractSequence implements Sequence, PositionContainer
{
  /** Normally the Sequence this a sub-sequence of.
   * Actually the sequence that provides context for the
   * start and end position pairs. */
  AbstractSequence base;

  /** Integer part of start position. */
  int ipos0;

  /** Pointer part of start position. */
  Object xpos0;

  /** Integer part of end position. */
  int ipos1;

  /** Pointer part of end position. */
  Object xpos1;

  public SubSequence()
  {
  }

  public SubSequence(AbstractSequence base)
  {
    this.base = base;
  }

  public Object get (int index)
  {
    if (index < 0)
      throw new IndexOutOfBoundsException();
    int start = base.nextIndex(ipos0, xpos0);
    return base.get(start + index);
  }

  public int size()
  {
    return base.getIndexDifference(ipos1, xpos1, ipos0, xpos0);
  }

  public void remove(int ipos0, Object xpos0, int ipos1, Object xpos1)
  {
    base.remove(ipos0, xpos0, ipos1, xpos1);
  }

  public int getPositionInt(int positionNumber)
  { return positionNumber == 0 ? ipos0 : ipos1; }
  public Object getPositionPtr(int positionNumber)
  { return positionNumber == 0 ? xpos0 : xpos1; }
  public void setPosition(int positionNumber, int ipos, Object xpos)
  {
    if (positionNumber == 0)
      { ipos0 = ipos;  xpos0 = xpos; }
    else
      { ipos1 = ipos;  xpos1 = xpos; }
  }
  public void setSequence(int positionNumber, AbstractSequence seq) { }
  public int countPositions() { return 2; }

  protected boolean isAfter(int ipos, Object xpos)
  { return base.isAfter(ipos, xpos); }

  public void makePosition(int offset, boolean isAfter,
			   PositionContainer posSet, int posNumber)
  {
    base.makeRelativePosition(ipos0, xpos0, offset, isAfter,
                             posSet, posNumber);
  }

  public void makeRelativePosition(int istart, Object xstart,
                                  int offset, boolean isAfter,
                                  PositionContainer posSet,
                                  int posNumber)
  {
    base.makeRelativePosition(istart, xstart, offset, isAfter,
                             posSet, posNumber);
  }

  protected int getIndexDifference(int ipos1, Object xpos1,
                                 int ipos0, Object xpos0)
  {
    return base.getIndexDifference(ipos1, xpos1, ipos0, xpos0);
  }

  public void releasePosition(int ipos, Object xpos)
  {
    base.releasePosition(ipos, xpos);
  }

  protected int nextIndex(int ipos, Object xpos)
  {
    return getIndexDifference(ipos, xpos, ipos0, xpos0);
  }

  public int compare(int ipos1, Object xpos1, int ipos2, Object xpos2)
  {
    return base.compare(ipos1, xpos1, ipos2, xpos2);
  }

  protected Object getNext(int ipos, Object xpos)
  {
    if (base.compare(ipos, xpos, ipos1, xpos1) >= 0)
      return eofValue;
    return base.getNext(ipos, xpos);
  }

  public int getNextKind(int ipos, Object xpos)
  {
    if (base.compare(ipos, xpos, ipos1, xpos1) >= 0)
      return EOF_VALUE;
    return base.getNextKind(ipos, xpos);
  }

  protected Object getPrevious(int ipos, Object xpos)
  {
    if (base.compare(ipos, xpos, ipos0, xpos0) <= 0)
      return eofValue;
    return base.getPrevious(ipos, xpos);
  }

  public void clear()
  {
    remove(ipos0, xpos0, ipos1, xpos1);
  }

  public void finalize()
  {
    base.releasePosition(ipos0, xpos0);
    base.releasePosition(ipos1, xpos1);
  }
}
