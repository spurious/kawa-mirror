// Copyright (c) 2001, 2003, 2005, 2007  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import gnu.text.Char;

/** Editable character sequence using a buffer-gap implementation and
 * self-adjusting position.
 * Can implement (the text part of) an Emacs buffer, or a
 * javax.swing.text.AbstractDocument.Content
 */

public class CharBuffer extends FString
{
    public CharBuffer(FString str) {
        super((CharSequence) str);
        indexes = new StableManager(this);
    }

    public CharBuffer(int initialSize) {
        super(initialSize);
        indexes = new StableManager(this);
    }

    protected CharBuffer() {
        indexes = new StableManager(this);
    }

    public int length() { return size(); }

    /* REDUNDANT - but perhaps needs to be optimized
  public String substring (int start, int end)
  {
    int sz = size();
    if (start < 0 || end < start || end > sz)
      throw new IndexOutOfBoundsException();
    int len = end - start;
    start = getSegment(start, len);
    return new String(getArray(), start, len);
  }
    */

    /* REDUNDANT - but perhaps needs to be optimized
  public CharSeq subSequence(int start, int end)
  {
    int sz = size();
    if (start < 0 || end < start || end > sz)
      throw new IndexOutOfBoundsException();
    return SubCharSeq.valueOf(this,
                          base.createPos(start, false),
                          base.createPos(end, true));
  }
*/

    /* REDUNDANT - but perhaps needs to be optimized
  public void fill(int fromIndex, int toIndex, char value)
  {
    char[] array = string.data;
    int i = fromIndex;
    int limit = gapStart < toIndex ? gapStart : toIndex;
    for (;  i < limit;  i++)
      array[i] = value;
    int gapSize = gapEnd - gapStart;
    i = limit + gapSize;
    limit += toIndex;
    for (;  i < limit;  i++)
      array[i] = value;
  }

  / ** Set all the elements to a given character. * /
  public final void fill (char value)
  {
    char[] array = string.data;
    for (int i = array.length;  --i >= gapEnd; )
      array[i] = value;
    for (int i = gapStart;  --i >= 0; )
      array[i] = value;
  }
    */

  public char[] getArray() { return (char[]) getBuffer(); }

  public void delete(int start, int end)
  {
    int ipos = createPos(start, false);
    removePos(ipos, end-start);
    releasePos(ipos);
  }

  public void consume(int start, int count, Consumer dest)
  {
      throw new Error();
      /*
    char[] array = data;
    if (start < gapStart)
      {
	int count0 = gapStart - start;
	if (count0 > count)
	  count0 = count;
	dest.write(array, start, count0);
	count -= count0;
	start += count;
      }
    if (count > 0)
      {
	start += gapEnd - gapStart;
	dest.write(array, start, count);
      }
      */
  }

  public String toString()
  {
    int len = size();
    int start = getSegment(0, len);
    return new String(getArray(), start, len);
  }

  /* #ifdef JAVA5 */
  public void writeTo(int start, int count, Appendable dest)
    throws java.io.IOException
  {
    if (dest instanceof java.io.Writer)
      writeTo(start, count, (java.io.Writer) dest);
    else
      dest.append(this, start, start+count);
  }

  public void writeTo(Appendable dest)
    throws java.io.IOException
  {
    writeTo(0, size(), dest);
  }
  /* #endif */

  public void dump()
  {
    System.err.println("Buffer Content dump.  size:"+size()+"  buffer:"+getArray().length);
    StableManager manager = (StableManager) indexes;
    int gapStart = manager.getGapStart();
    int gapEnd = manager.getGapEnd();
    int[] positions = manager.positions;
    int free = manager.free;
    System.err.print("before gap: \"");
    System.err.print(new String(getArray(), 0, gapStart));
    System.err.println("\" (gapStart:"+gapStart+" gapEnd:"+gapEnd+')');
    System.err.print("after gap: \"");
    System.err.print(new String(getArray(), gapEnd, getArray().length-gapEnd));
    System.err.println("\"");
    int poslen = positions == null ? 0 : positions.length;
    System.err.println("Positions (size: "+poslen+" free:"+free+"):");
    boolean[] isFree = null;
    if (free != -2)
      {
        isFree = new boolean[positions.length];
        for (int i = free;  i >= 0;  i = positions[i])
          isFree[i] = true;
      }
    for (int i = 0;  i < poslen;  i++)
      {
	int pos = positions[i];
	if (free == -2 ? pos != StableManager.FREE_POSITION : ! isFree[i]) {
            int p = pos>>1;
            if (p > gapStart)
                p -= gapEnd-gapStart;
	  System.err.println("position#"+i+": "+p+" isAfter:"+(pos&1));
        }
      }
  }

    // Needed for SwingBuffer:
    public int nextIndex(int ipos) { return super.nextIndex(ipos); }
    public void releasePos(int ipos) { super.releasePos(ipos); }
}
