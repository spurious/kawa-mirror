// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** Editable character sequence using a a buffer-gap implementstion and
 * self-adjusting position.
 * Can implement (the text part of) an Emacs buffer, or a
 * javax.swing.text.AbstractDocument.Content
 */

public class CharBuffer extends StableVector implements CharSequence
{
  private FString string;

  public CharBuffer(FString str)
  {
    super(str);
    string = str;
  }

  public CharBuffer(int initialSize)
  {
    this(new FString(initialSize));
  }

  public int length() { return size(); }

  public char charAt(int index)
  {
    // If index is out of bounds, the base.get will catch that.
    if (index >= gapStart)
      index += gapEnd - gapStart;
    return string.charAt(index);
  }

  /** Copy characters into a destination buffer.
   * Same interface as java.lang.String's getChars. */
  public void getChars (int srcBegin, int srcEnd, char[] dst, int dstBegin)
  {
    char[] array = string.data;
    int count;
    if (srcBegin < gapStart)
      {
	count = (srcEnd < gapStart ? srcEnd : gapStart) - srcBegin;
	if (count > 0)
	  {
	    System.arraycopy(array, srcBegin, dst, dstBegin, count);
	    srcBegin += count;
	    dstBegin += count;
	  }
      }
    int gapSize = gapEnd - gapStart;
    srcBegin += gapSize;
    srcEnd += gapSize;
    count = srcEnd - srcBegin;
    if (count > 0)
      System.arraycopy(array, srcBegin, dst, dstBegin, count);
  }

  public void setCharAt(int index, char value)
  {
    // If index is out of bounds, the base.get will catch that.
    if (index >= gapStart)
      index += gapEnd - gapStart;
    string.setCharAt(index, value);
  }

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

  /** Set all the elements to a given character. */
  public final void fill (char value)
  {
    char[] array = string.data;
    for (int i = array.length;  --i >= gapEnd; )
      array[i] = value;
    for (int i = gapStart;  --i >= 0; )
      array[i] = value;
  }

  public char[] getArray() { return (char[]) base.getBuffer(); }

  public void delete(int where, int count)
  {
    int ipos = createPosition(where, false);
    remove(ipos, null, count);
    releasePosition(ipos, null);
  }

  public void insert(int where, String str, boolean beforeMarkers/*ignored*/)
  {
    int len = str.length();
    gapReserve(where, len);
    str.getChars(0, len, string.data, where);
    gapStart += len;
  }

  public void consume(int start, int count, Consumer dest)
  {
    char[] array = string.data;
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
  }

  public void writeTo(int start, int count, java.io.Writer dest)
    throws java.io.IOException
  {
    char[] array = string.data;
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
  }

  public void writeTo(java.io.Writer dest) throws java.io.IOException
  {
    char[] array = string.data;
    dest.write(array, 0, gapStart);
    dest.write(array, gapEnd, array.length - gapEnd);
  }
}
