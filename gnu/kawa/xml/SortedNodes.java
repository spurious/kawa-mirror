// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.Values;
import gnu.lists.*;
import gnu.xml.*;

/** Manages a sequence of node references in document order without duplicates.
 * All elements are POSITION_PAIR_FOLLOWS elements, which makes operations
 * simple and efficient.  The most recently added element is just before
 * the gap. Optimized for the data being in order, or at least having good
 * locality (a node being "near" the previously-entered node). */

public class SortedNodes extends Values
{
  /** Number of data elements for a POSITION_PAIR_FOLLOWS node reference. */
  static final int POS_SIZE = 5;

  int count;

  int nesting = 0;
  NodeTree curFragment;

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
   * Require index>=start && index<end, where end==start+POS_SIZE*count.
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
	int mid = (lo + hi) >> 1;
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

  public boolean writePosition(AbstractSequence seq, int ipos)
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
	      return true;
	    int delta = i - gapEnd;
	    if (delta > 0)
	      {
		System.arraycopy(data, gapEnd, data, gapStart, delta);
		gapEnd = i;
		gapStart += delta;
	      }
	  }
	else if (cmp == 0)
	  return true;
	else
	  {
	    int i = find (0, lastIndex / POS_SIZE, seq, ipos);
	    if (i < 0)
	      return true;
	    int delta = gapStart - i;
	    if (delta > 0)
	      {
		System.arraycopy(data, i, data, gapEnd - delta, delta);
		gapStart = i;
		gapEnd -= delta;
	      }
	  }
      }
    count++;
    return super.writePosition(seq, ipos);
  }

  public int find (Object seq)
  {
    // See if can re-use the object index of the position before the gap.
    if (gapStart > 0)
      {
	int oindex = getIntN(gapStart - POS_SIZE + 1);
	if (objects[oindex] == seq)
	  return oindex;
      }
    // See if can re-use the object index of the position after the gap.
    if (gapEnd < data.length)
      {
	int oindex = getIntN(gapEnd + 1);
	if (objects[oindex] == seq)
	  return oindex;
      }
    return super.find(seq);
  }

  public void writeObject(Object v)
  {
    if (curFragment != null)
      {
	if (nesting == 0
	    && (v instanceof SeqPosition || v instanceof TreeList))
	  finishFragment();
	else
	  {
	    curFragment.writeObject(v);
	    return;
	  }
      }
    if (v instanceof SeqPosition)
      {
	SeqPosition seq = (SeqPosition) v;
	writePosition(seq.sequence, seq.ipos);
	return;
      }
    if (v instanceof TreeList)
      {
	TreeList tlist = (TreeList) v;
	writePosition(tlist, 0);
	return;
      }
    startFragment();
    curFragment.writeObject(v);
    return;
  }

  void maybeStartTextNode ()
  {
    if (curFragment == null)
      {
	startFragment();
	// OR: throw some exception.
      }
  }

  public void writeFloat (float v)
  {
    maybeStartTextNode();
    curFragment.writeFloat(v);
  }

  public void writeDouble (double v)
  {
    maybeStartTextNode();
    curFragment.writeDouble(v);
  }

  public void writeLong(long v)
  {
    maybeStartTextNode();
    curFragment.writeLong(v);
  }

  public void writeInt(int v)
  {
    maybeStartTextNode();
    curFragment.writeInt(v);
  }

  public void writeChar (int v)
  {
    maybeStartTextNode();
    curFragment.writeChar(v);
  }

  public void writeBoolean (boolean v)
  {
    maybeStartTextNode();
    curFragment.writeBoolean(v);
  }

  public void writeChars (String str)
  {
    maybeStartTextNode();
    curFragment.writeChars(str);
  }

  public void write(char[] buf, int off, int len)
  {
    maybeStartTextNode();
    curFragment.write(buf, off, len);
  }

  private void maybeStartNonTextNode ()
  {
    if (curFragment != null && nesting == 0)
      finishFragment();
    if (curFragment == null)
      startFragment();
    nesting++;
  }

  private void maybeEndNonTextNode ()
  {
    if (--nesting == 0)
      finishFragment();
  }

  public void beginGroup(String typeName, Object type)
  {
    maybeStartNonTextNode();
    curFragment.beginGroup(typeName, type);
  }

  public void endGroup(String typeName)
  {
    curFragment.endGroup(typeName);
    maybeEndNonTextNode();
  }

  public void beginAttribute(String attrName, Object attrType)
  {
    maybeStartNonTextNode();
    curFragment.beginAttribute(attrName, attrType);
  }

  public void endAttribute()
  {
    curFragment.endAttribute();
    maybeEndNonTextNode();
  }

  public void beginDocument()
  {
    maybeStartNonTextNode();
    curFragment.beginDocument();
  }

  public void endDocument()
  {
    curFragment.endDocument();
    maybeEndNonTextNode();
  }

  void startFragment ()
  {
    curFragment = new NodeTree();
    writePosition(curFragment, 0);
  }

  void finishFragment ()
  {
    curFragment = null;
  }

  public int size()
  {
    return count;
  }

  public Object get (int index)
  {
    int i = POS_SIZE * index;
    if (i > gapStart)
      i += gapEnd - gapStart;
    if (i < 0 || i >= data.length)
      throw new IndexOutOfBoundsException();
    // Inline of: return getPosNext(i << 1)
    if (data[i] != POSITION_PAIR_FOLLOWS)
      throw new RuntimeException("internal error - unexpected data");
    return SeqPosition.make((AbstractSequence) objects[getIntN(i+1)],
			    getIntN(i+3));
  }
}
