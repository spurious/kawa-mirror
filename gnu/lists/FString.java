// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector whose elements are 32-bit floats.
 * Used for the Scheme string type.
 * @author Per Bothner
 */

public class FString extends SimpleVector
implements CharSequence, Externalizable, Consumable
{
  public char[] data;
  protected static char[] empty = new char[0];

  public FString ()
  {
    data = empty;
  }

  public FString (int num)
  {
    size = num;
    data = new char[num];
  }

  public FString (int num, char value)
  {
    char[] array = new char[num];
    data = array;
    size = num;
    while (--num >= 0)
      array[num] = value;
  }

  /** Create an FString from a char[].
   * Note that this contructor does *not* copy the argument. */
  public FString (char[] values)
  {
    size = values.length;
    data = values;
  }

  public FString (String str)
  {
    data = str.toCharArray();
    size = data.length;
  }

  public FString (StringBuffer buffer)
  {
    this(buffer, 0, buffer.length());
  }

  public FString (StringBuffer buffer, int offset, int length)
  {
    this.size = length;
    data = new char[length];
    if (length > 0)
      buffer.getChars (offset, offset+length, data, 0);
  }

  public FString (char[] buffer, int offset, int length)
  {
    this.size = length;
    data = new char[length];
    System.arraycopy(buffer, offset, data, 0, length);
  }

  public FString(Sequence seq)
  {
    this.data = new char[seq.size()];
    addAll(seq);
  }

  public FString(CharSequence seq)
  {
    int size = seq.size();
    char[] data = new char[size];
    seq.getChars(0, size, data, 0);
    this.data = data;
    this.size = size;
  }

  public FString(CharSequence seq, int offset, int length)
  {
    char[] data = new char[length];
    seq.getChars(offset, offset+length, data, 0);
    this.data = data;
    this.size = length;
  }

  public int length() { return size; }

  /** Get the allocated length of the data buffer. */
  public int getBufferLength()
  {
    return data.length;
  }

  public void setBufferLength(int length)
  {
    int oldLength = data.length;
    if (oldLength != length)
      {
	char[] tmp = new char[length];
	System.arraycopy(data, 0, tmp, 0,
			 oldLength < length ? oldLength : length);
	data = tmp;
      }
  }

  protected Object getBuffer() { return data; }

  public final Object getBuffer(int index)
  {
    return Convert.toObject(data[index]);
  }

  public final Object setBuffer(int index, Object value)
  {
    Object old = Convert.toObject(data[index]);
    data[index] = Convert.toChar(value);
    return old;
  }

  public final Object get (int index)
  {
    if (index >= size)
      throw new ArrayIndexOutOfBoundsException();
    return Convert.toObject(data[index]);
  }

  public final char charAt(int index)
  {
    if (index >= size)
      throw new StringIndexOutOfBoundsException(index);
    return data[index];
  }

  public final char charAtBuffer(int index)
  {
    return data[index];
  }

  public void getChars (int srcBegin, int srcEnd, char dst[], int dstBegin)
  {
    if (srcBegin < 0 || srcBegin > srcEnd)
      throw new StringIndexOutOfBoundsException(srcBegin);
    if (srcEnd > size)
      throw new StringIndexOutOfBoundsException(srcEnd);
    if (dstBegin+srcEnd-srcBegin > dst.length)
      throw new StringIndexOutOfBoundsException(dstBegin);
    if (srcBegin < srcEnd)
      System.arraycopy(data, srcBegin, dst, dstBegin, srcEnd - srcBegin);
  }

  /** Return a char[] contain the characters of this string.
   * It is unspecified if the result is a copy or shares with this FString.
   */
  public char[] toCharArray()
  {
    int val_length = data.length;
    int seq_length = size;
    if (seq_length == val_length)
      return data;
    else
      {
	char[] tmp = new char[seq_length];
	System.arraycopy(data, 0, tmp, 0, seq_length);
	return tmp;
      }
  }

  public void shift(int srcStart, int dstStart, int count)
  {
    System.arraycopy(data, srcStart, data, dstStart, count);
  }

  public FString copy (int start, int end)
  {
    char[] copy = new char[end-start];
    for (int i = start;  i < end;  i++)
      copy[i-start] = data[i];
    return new FString(copy);
  }

  public String toString ()
  {
    return new String (data, 0, size);
  }

  public String substring(int start, int end)
  {
    return new String (data, start, end - start);
  }

  public void setCharAt (int index, char ch)
  {
    if (index < 0 || index >= size)
      throw new StringIndexOutOfBoundsException(index);
    data[index] = ch;
  }

  public void setCharAtBuffer (int index, char ch)
  {
    data[index] = ch;
  }

  /** Set all the elements to a given character. */
  public final void fill (char ch)
  {
    for (int i = size;  --i >= 0; )
      data[i] = ch;
  }

  public void fill(int fromIndex, int toIndex, char value)
  {
    if (fromIndex < 0 || toIndex > size)
      throw new IndexOutOfBoundsException();
    for (int i = fromIndex;  i < toIndex;  i++)
      data[i] = value;
  }

  protected void clearBuffer(int start, int count)
  {
    while (--count >= 0)
      data[start++] = 0;
  }

  public void replace(int where, char[] chars, int start, int count)
  {
    System.arraycopy(chars, start, data, where, count);
  }

  public void replace(int where, String string)
  {
    string.getChars(0, string.length(), data, where);
  }

  public int hashCode ()
  {
    /* Matches String.hashCode specification, as updated specification in
       http://www.javasoft.com/docs/books/jls/clarify.html. */
    char[] val = data;
    int len = size;
    int hash = 0;
    for (int i = 0;  i < len;  i++)
      hash = 31 * hash + val[i];
    return hash;
  }

  public boolean equals (Object obj)
  {
    if (obj == null || !(obj instanceof FString))
      return false;
    char[] str = ((FString) obj).data;
    int n = size;
    if (str == null || str.length != n)
      return false;
    for (int i = n;  --i >= 0; )
      {
	if (data[i] != str[i])
	  return false;
      }
    return true;
  }

  public void consume(Consumer out)
  {
    String typeName = "#text"; 
    String type = typeName;
    out.beginGroup(typeName, type);
    out.endAttributes();
    out.write(data, 0, data.length);
    out.endGroup(typeName);
  }

  protected void consume(int iposStart, Object xposStart,
			 int iposEnd, Object xposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    out.write(data, i, end - i);
  }

  public void writeTo(int start, int count, java.io.Writer dest)
    throws java.io.IOException
  {
    dest.write(data, start, count);
  }

  public void writeTo(java.io.Writer dest) throws java.io.IOException
  {
    dest.write(data, 0, size);
  }

  /**
   * @serialData Write 'size' (using writeInt),
   * followed by 'size' elements in order (using writeChar).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int size = this.size;
    out.writeInt(size);
    for (int i = 0;  i < size;  i++)
      out.writeChar(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int size = in.readInt();
    char[] data = new char[size];
    for (int i = 0;  i < size;  i++)
      data[i] = in.readChar();
    this.data = data;
    this.size = size;
  }
}
