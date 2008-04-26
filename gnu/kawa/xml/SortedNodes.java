// Copyright (c) 2003, 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.xml.*;

/** Manages a sequence of node references in document order without duplicates.
 * All elements are POSITION_PAIR_FOLLOWS elements, which makes operations
 * simple and efficient.  The most recently added element is just before
 * the gap. Optimized for the data being in order, or at least having good
 * locality (a node being "near" the previously-entered node). */

public class SortedNodes extends Nodes
{
  int nesting = 0;

  int compareIndex(int index, AbstractSequence seq2, int ipos2)
  {
    int datum = data[index];
    if (datum != POSITION_PAIR_FOLLOWS)
      throw new RuntimeException("invalid kind of value to compare");
    AbstractSequence seq = (AbstractSequence) objects[getIntN(index+1)];
    return AbstractSequence.compare(seq, getIntN(index+3),
				    seq2, ipos2);
  }

  /** Find index where to put position (seq, ipos).
   * Require {@code index>=start && index<end},
   * where {@code end==start+POS_SIZE*count}.
   * Require all position before index are "less than" (seq, ipos),
   * and all positions after are "greater than" (seq, ipos).
   * If there is no such index (because it is "same as"), return -1.
   */
  int find (int start, int count, AbstractSequence seq, int ipos)
  {
    int lo = 0;
    int hi = count;
    // We use binary search, though the arraycopy operations in writePosition
    // limit the value - a sequence of writePosition calls is still quadratic
    // in the worst case (but linear if locality is good).
    while (lo < hi)
      {
	int mid = (lo + hi) >>> 1;
	int cmp = compareIndex(start + POS_SIZE * mid, seq, ipos);
	if (cmp == 0)
	  return -1;
	if (cmp > 0)
	  hi = mid;
	else
	  lo = mid + 1;
      }
    return start + POS_SIZE * lo;
  }

  public void writePosition(AbstractSequence seq, int ipos)
  {
    if (count >  0)
      {
	int lastIndex = gapStart - POS_SIZE;
	int cmp = compareIndex(lastIndex, seq, ipos);
	if (cmp < 0)
	  {
	    // The new node is after all nodes up to gapStart.
	    int i = gapEnd;
	    int end = data.length;
	    // Note that if the incoming nodes are already sorted (a common
	    // case in path expressions), then find will immediately return i.
	    i = find (i, (end - i) / POS_SIZE, seq, ipos);
	    if (i < 0)
	      return;
	    int delta = i - gapEnd;
	    if (delta > 0)
	      {
		System.arraycopy(data, gapEnd, data, gapStart, delta);
		gapEnd = i;
		gapStart += delta;
	      }
	  }
	else if (cmp == 0)
	  return;
	else
	  {
	    int i = find (0, lastIndex / POS_SIZE, seq, ipos);
	    if (i < 0)
	      return;
	    int delta = gapStart - i;
	    if (delta > 0)
	      {
		System.arraycopy(data, i, data, gapEnd - delta, delta);
		gapStart = i;
		gapEnd -= delta;
	      }
	  }
      }
    super.writePosition(seq, ipos);
  }

}
