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
    StableManager manager;
    public CharBuffer(FString str) {
        super((CharSequence) str);
        manager = new StableManager(this);
    }

    public CharBuffer(int initialSize) {
        super(initialSize);
        setGapBounds(0, initialSize);
        manager = new StableManager(this);
    }

    protected CharBuffer() {
        manager = new StableManager(this);
    }

    public int length() { return size(); }

  public char[] getArray() { return (char[]) getBuffer(); }

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

    @Override public int startPos() { return manager.startPos(); }
    @Override public int endPos() { return manager.endPos(); }
    @Override public boolean isAfterPos(int ipos) { return manager.isAfterPos(ipos); }
    @Override public boolean hasNext(int ipos) { return manager.hasNext(ipos); }
    @Override public int nextPos(int ipos) { return manager.nextPos(ipos); }
    @Override public int copyPos(int ipos) { return manager.copyPos(ipos); }
    @Override public int nextIndex(int ipos) {
        return manager.nextIndex(ipos);
    }
    @Override public void releasePos(int ipos) { manager.releasePos(ipos); }

    @Override public int createPos(int index, boolean isAfter) {
        return manager.createPos(index, isAfter);
    }

    @Override
    public void insert(int where, int ch, boolean beforeMarkers) {
        super.insert(where, ch, beforeMarkers);
        if (beforeMarkers) {
            // Adjust markers at insertion point to be after inserted next.
            int len = ch >= 0x10000 ? 2 : 1;
            int oldPos = (getGapStart()-len) << 1;
            manager.adjustPositions(oldPos, oldPos + 1, len << 1);
        }
    }

    @Override
    public void insert(int where, String str, boolean beforeMarkers) {
        super.insert(where, str, beforeMarkers);
        if (beforeMarkers) {
            // Adjust markers at insertion point to be after inserted next.
            int len = str.length();
            int oldPos = (getGapStart()-len) << 1;
            manager.adjustPositions(oldPos, oldPos + 1, len << 1);
        }
    }

    @Override
    protected void gapReserve(int where, int needed) {
        manager.gapReserve(this, where, needed);
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
    int gapStart = getGapStart();
    int gapEnd = getGapEnd();
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
	  System.err.println("position#"+i+": [raw:"+pos+"]="+p+" isAfter:"+(pos&1));
        }
      }
  }

}
