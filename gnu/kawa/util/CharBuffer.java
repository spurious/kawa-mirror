package gnu.kawa.util;

public class CharBuffer extends AbstractString
{
  static final int START_POSITION = 0;
  static final int END_POSITION = 1;

  // These should be protected.  FIXME!
  public char[] array;
  public int gapStart;
  public int gapEnd;

  static final int[] noInts = { };
  protected int[] positions = noInts;

  public CharBuffer(int initialSize)
  {
    array = new char[initialSize];
    gapStart = 0;
    gapEnd = initialSize;
  }

  public CharBuffer (int num, char ch, int initialSize)
  {
    if (num > initialSize)
      initialSize = num;
    array = new char[initialSize];
    gapStart = num;
    gapEnd = initialSize;
    for (int i = num;  --i >= 0;)
      array[i] = ch;
  }

  public CharBuffer (int num, char ch)
  {
    this(num, ch, num);
  }

  public CharBuffer (char[] buffer, int offset, int length, int initialSize)
  {
    if (length > initialSize)
      initialSize = length;
    array = new char[initialSize];
    gapStart = 0;
    gapEnd = length;
    System.arraycopy(buffer, offset, array, 0, length);
  }

  public CharBuffer (char[] buffer, int offset, int length)
  {
    this(buffer, offset, length, length);
  }

  public final int length()
  {
    return array.length - (gapEnd - gapStart);
  }

  public final char charAt(int index)
  {
     if (index >= gapStart)
       index += gapEnd - gapStart;
     return array[index];
  }

  public void setCharAt (int index, char ch)
  {
     if (index >= gapStart)
       index += gapEnd - gapStart;
    array[index] = ch;
  }

  public void getChars (int srcBegin, int srcEnd, char[] dst, int dstBegin)
  {
    int count = srcEnd - srcBegin;
    if (srcBegin < gapStart)
      {
	int len = gapStart - srcBegin;
	if (len > count)
	  len = count;
	count -= len;
	System.arraycopy(array, srcBegin, dst, dstBegin, len);
	dstBegin += len;
	srcBegin += len;
      }
    srcBegin += gapEnd - gapStart;
    if (srcBegin >= gapEnd && count != 0)
      System.arraycopy(array, srcBegin, dst, dstBegin, count);
  }

  public void writeTo (int start, int count, java.io.Writer dest)
    throws java.io.IOException
  {
    if (start < gapStart)
      {
	int len = gapStart - start;
	if (count != -1)
	  {
	    if (len > count)
	      len = count;
	    count -= len;
	  }
	dest.write(array, start, len);
	start += len;
      }
    start += gapEnd - gapStart;
    if (start >= gapEnd)
      {
	int len = array.length - start;
	if (count != -1 && len > count)
	  len = count;
	dest.write(array, start, len);
      }
  }

  /** Set all the elements to a given character. */
  public final void fill (char ch)
  {
    for (int i = array.length;  --i >= gapEnd; )
      array[i] = ch;
    for (int i = gapStart;  --i >= 0; )
      array[i] = ch;
  }

  public void replace(int where, char[] chars, int start, int count)
  {
    if (where + count > gapStart)
      {
	if (where >= gapStart)
	  where += gapEnd - gapStart;
	else
	  shiftGap(where + count);
      }
    System.arraycopy(chars, start, array, where, count);
  }

  public void replace(int where, String string)
  {
    int count = string.length();
    if (where + count > gapStart)
      {
	if (where >= gapStart)
	  where += gapEnd - gapStart;
	else
	  shiftGap(where + count);
      }
    string.getChars(0, count, array, where);
  }

  public void delete(int where, int count)
  {
    // Adjust the positions.
    int adjust = count << 2;
    int pos0 = (where) << 2;
    int pos1 = (where + 1) << 2;
    int pos2 = (where + count) << 2;
    for (int i = positions.length;  --i >= 0; )
      {
	int off = positions[i];
	if (off < pos1)
	  continue;
	if (off >= pos2)
	  positions[i] = off - adjust;
	else
	  positions[i] = pos0 | (off & 3);
      }

    if (where != gapStart)
      shiftGap(where);
    gapEnd += count;
  }

  /** Insert count unspecified (garbage) characters at where.
   * Moves the gapStart after the inserted characters. */
  protected void insert(int where, int count, boolean beforeMarkers)
  {
    // Adjust the positions.
    int pos = where<<2;
    if (! beforeMarkers)
      pos += EMACS_MARK_KIND;
    int adjust = count << 2;
    for (int i = positions.length;  --i >= 0; )
      {
	int off = positions[i];
	if (off > pos)
	  positions[i] = off + adjust;
      }

    gapReserve(where, count);
    gapStart += count;
  }

  protected void shiftGap(int newGapStart)
  {
    int delta = newGapStart - gapStart;
    if (delta > 0)
      {
        System.arraycopy(array, gapEnd, array, gapStart, delta);
      }
    else if (delta < 0)
      {
        System.arraycopy(array, newGapStart, array, gapEnd + delta, - delta);
      }
    gapEnd += delta;
    gapStart = newGapStart;
  }

  /** Adjust gap to where, and make sure it is least `size' chars long. */
  protected void gapReserve(int where, int size)
  {
    int oldlen = length();
    // if (where < 0 || where > oldlen)
    //   throw new BadLocationException("bad location (in gapReserve)", where);
    if (oldlen + size > array.length)
      {
        int newlen;
        if (oldlen < 20000)
          newlen = 2 * oldlen;
        else
          newlen = oldlen + 4000;
        int minlen = oldlen + size + 100;
        if (minlen > newlen)
          newlen = minlen + 500;
        char[] newarray = new char[newlen];
        if (where <= gapStart)
          {
            System.arraycopy(array, 0, newarray, 0, where);
            int moveSize = gapStart - where;
            int endSize = array.length - gapEnd;
	    int newGapEnd = newlen - endSize - moveSize;
	    System.arraycopy(array, where, newarray, newGapEnd, moveSize);
	    System.arraycopy(array, gapEnd, newarray, newlen-endSize, endSize);
            gapEnd = newGapEnd;
          }
        else
          {
            System.arraycopy(array, 0, newarray, 0, gapStart);
            System.arraycopy(array, gapEnd, newarray, gapStart,
                             where - gapStart);
            int whereOffset = where + (gapEnd - gapStart);
            int endSize = array.length - whereOffset;
            gapEnd = newlen - endSize;
            System.arraycopy(array, whereOffset, newarray, gapEnd, endSize);
          }
	array = newarray;
        gapStart = where;
      }
    else if (where != gapStart)
      {
        shiftGap(where);
      }
  }

  public synchronized int createPosition (int offset, int kind)
  {
    int position;
    int poslen = positions.length;
    if (poslen == 0)
      {
	poslen = 10;
	positions = new int[poslen];
	positions[START_POSITION] = (0 << 2) | BEFORE_MARK_KIND;
	positions[END_POSITION] = (length() << 2) | AFTER_MARK_KIND;
      }
    if (kind == BEFORE_MARK_KIND && offset == 0)
      return START_POSITION;
    if (kind == AFTER_MARK_KIND && offset == length())
      return END_POSITION;
    for (position = 2;  position < poslen;  position++)
      if (positions[position] == 0)
	break;
    if (position >= poslen)
      {
	int[] newpos = new int [poslen * 2];
	System.arraycopy(positions, 0, newpos, 0, poslen);
	positions = newpos;
      }
    positions[position] = (offset << 2) + kind;
    return position;
  }

  public void releasePosition (int position)
  { 
    if (position > 1)
      positions[position] = 0;
  }

  public int getPositionOffset (int position)
  {
    return positions[position] >> 2;
  }

  public int getPositionKind (int position)
  {
    return positions[position] & 3;
  }

  public int copyPosition (int position)
  {
    if (position == 0)
      return position;
    return createPosition(getPositionOffset(position),
			  getPositionKind(position));
  }

  public String toString ()
  {
    if (gapStart == 0)
      return new String(array, gapEnd, array.length - gapEnd);
    int tail = array.length - gapEnd;
    if (tail != 0)
      shiftGap(tail + gapStart);
    return new String(array, 0, gapStart);
  }
}
