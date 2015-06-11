// Copyright (c) 2001, 2004  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import gnu.text.Char;
import gnu.text.Char;

/** Simple adjustable-length vector whose elements are 32-bit code points
 * Used for the Scheme string type.
 * @author Per Bothner
 */

public class FString extends AbstractCharVector<Char>
    implements Appendable, CharSeq, Consumable
{
  public FString ()
  {
    data = empty;
  }

  public FString (int num)
  {
    size = num;
    data = new char[num];
  }

    public FString(int num, int value) {
        data = new char[value < 0x10000 ? num : 2 * num];
        insertRepeated(0, value, num);
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

    public FString (StringBuilder buffer) {
        this(buffer, 0, buffer.length());
    }

    public FString (StringBuilder buffer, int offset, int length) {
        this.size = length;
        data = new char[length];
        if (length > 0)
            buffer.getChars(offset, offset+length, data, 0);
    }

    public FString (StringBuffer buffer) {
        this(buffer, 0, buffer.length());
    }

    public FString (StringBuffer buffer, int offset, int length) {
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

    public FString(CharSequence seq) {
        this(seq, 0, seq.length());
    }

    public FString(CharSequence seq, int offset, int length) {
        char[] data = new char[length];
        if (seq instanceof CharSeq)
            ((CharSeq) seq).getChars(offset, offset+length, data, 0);
        else if (seq instanceof String)
            ((String) seq).getChars(offset, offset+length, data, 0);
        else {
            for (int i = length; --i >= 0; )
                data[i] = seq.charAt(offset+i);
        }
        this.data = data;
        this.size = length;
    }

    /** Create a empty string, but with a given initial buffer size. */
    public static FString alloc(int sz) {
        FString str = new FString(sz);
        str.size = 0;
        return str;
    }

  // Undefined if index>=size (unlike for other SimpleVector classes).
  public final Char getBuffer(int index) {
      return Char.valueOf(characterAt(index));
  }

  public final Char get(int index) {
      return Char.valueOf(characterAt(index));
  }

  // Undefined if index>=size (unlike for other SimpleVector classes).
  @Override
  public final void setBuffer(int index, Char value)
  {
      setCharacterAt(index, value.intValue());
  }

    public final int characterAt(int index) {
        // Maybe inline? FIXME
        return Strings.characterAt(this, 0, size, index);
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

  public void getChars (int srcBegin, int srcEnd, StringBuffer dst)
  {
    if (srcBegin < 0 || srcBegin > srcEnd)
      throw new StringIndexOutOfBoundsException(srcBegin);
    if (srcEnd > size)
      throw new StringIndexOutOfBoundsException(srcEnd);
    if (srcBegin < srcEnd)
      dst.append(data, srcBegin, srcEnd - srcBegin);
  }

  public void getChars (StringBuffer dst)
  {
    dst.append(data, 0, size);
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
    char[] src = data; // Move to local to help optimizer.
    for (int i = start;  i < end;  i++)
      copy[i-start] = src[i];
    return new FString(copy);
  }

  /** Append all the characters of another <code>FString</code>. */
  public boolean addAll(FString s)
  {
    int newSize = size + s.size;
    if (data.length < newSize)
      setBufferLength(newSize);
    System.arraycopy(s.data, 0, data, size, s.size);
    size = newSize;
    return s.size > 0;
  }

  public boolean addAll (CharSequence s)
  {
    int ssize = s.length();
    int newSize = size + ssize;
    if (data.length < newSize)
      setBufferLength(newSize);
    if (s instanceof FString)
      System.arraycopy(((FString) s).data, 0, data, size, ssize);
    else if (s instanceof String)
      ((String) s).getChars(0, ssize, data, size);
    else if (s instanceof CharSeq)
      ((CharSeq) s).getChars(0, ssize, data, size);
    else
      for (int i = ssize; --i >= 0; )
        data[size+i] = s.charAt(i);
    size = newSize;
    return ssize > 0;
  }

  /** Append arguments to this FString.
   * Used to implement Scheme's string-append.
   * @param args an array of FString value
   * @param startIndex index of first string in <code>args</code> to use
   */
  public void addAllStrings(Object[] args, int startIndex)
  {
    int total = size;
    for (int i = startIndex; i < args.length; ++i)
      {
        Object arg = args[i];
        total += ((CharSequence) arg).length();
      }
    if (data.length < total)
      setBufferLength(total);
    
    for (int i = startIndex; i < args.length; ++i)
      {
        addAll((CharSequence) args[i]);
      }
  }
  
  public String toString ()
  {
    return new String (data, 0, size);
  }

  public String substring(int start, int end)
  {
    return new String (data, start, end - start);
  }

  public CharSeq subSequence(int start, int end)
  {
    return new FString(data, start, end-start);
  }

  public void setCharAt (int index, char ch)
  {
    if (index < 0 || index >= size)
      throw new StringIndexOutOfBoundsException(index);
    data[index] = ch;
  }

    public void setCharacterAt(int index, int ch) {
        if (index < 0 || index >= size)
            throw new StringIndexOutOfBoundsException(index);
        char old1 = data[index];
        char old2;
        boolean oldIsSupp = old1 >= 0xD800 && old1 <= 0xDBFF
            && index+1 < size
            && (old2 = data[index+1]) >= 0xDC00 && old2 <= 0xDFFF;
        if (ch <= 0xFFFF) {
            if (oldIsSupp) {
                System.arraycopy(data, index+2, data, index+1, size-index-2);
                size--;
            }
            data[index] = (char) ch;
        } else if (ch == Char.IGNORABLE_CHAR) {
            int shrinkage = oldIsSupp ? 2 : 1;
            System.arraycopy(data, index+shrinkage,
                             data, index,
                             size-index-shrinkage);
            size -= shrinkage;
        } else {
            char c1 = (char) (((ch - 0x10000) >> 10) + 0xD800);
            char c2 = (char) ((ch & 0x3FF) + 0xDC00);
            if (!oldIsSupp) {
                ensureBufferLength(size+1);
                System.arraycopy(data, index+1, data, index+2, size-index-1);
                size++;
            }
            data[index] = c1;
            data[index+1] = c2;
        }
    }

    /** Replace a substring of this string with another.
     * The two strings may have different lengths, so this
     * generalizes insertion and deletion.
     */
    public void replace(CharSequence src, int srcStart, int srcEnd,
                        int dstStart, int dstEnd) {
        if (dstStart < 0 || dstStart > dstEnd || dstEnd > size
            || srcStart < 0 || srcStart > srcEnd || srcEnd > src.length())
            throw new StringIndexOutOfBoundsException();
        int srcLength = srcEnd - srcStart;
        int dstLength = dstEnd - dstStart;
        int grow = srcLength - dstLength;
        if (grow != 0) {
            if (grow > 0)
                ensureBufferLength(size + grow);
            System.arraycopy(data, dstEnd, data, dstEnd+grow, size-dstEnd);
            size += grow;
        }
        if (dstStart <= srcStart) {
            int i = dstStart;
            int j = srcStart;
            for (; j < srcEnd; i++, j++) {
                data[i] = src.charAt(j);
            }
        } else {
            int i = dstEnd + grow;
            int j = srcEnd;
            while (--j >= srcStart) {
                data[--i] = src.charAt(j);
            }
        }
    }

  public void setCharAtBuffer (int index, char ch)
  {
    data[index] = ch;
  }

  /** Set all the elements to a given character. */
  public final void fill (char ch)
  {
    char[] d = data; // Move to local to help optimizer.
    for (int i = size;  --i >= 0; )
      d[i] = ch;
  }

  public void fill(int fromIndex, int toIndex, char value)
  {
    if (fromIndex < 0 || toIndex > size)
      throw new IndexOutOfBoundsException();
    char[] d = data; // Move to local to help optimizer.
    for (int i = fromIndex;  i < toIndex;  i++)
      d[i] = value;
  }

    public void delete(int start, int end) {
        int len = size;
        if (len != end)
            System.arraycopy(data, end, data, start, len-end);
        size -= end-start;
    }

    public void insertRepeated(int where, int value, int count) {
        char c1, c2;
        int len;
        if (value >= 0x10000) {
            c1 = (char) (((value - 0x10000) >> 10) + 0xD800);
            c2 = (char) ((value & 0x3FF) + 0xDC00);
            len = 2 * count;
        } else {
            c1 = (char) value;
            c2 = 0;
            len = count;
        }
        addSpace(where, len);
        char[] array = data;
        int end = where + len;
        for (int i = where;  i < end;  ) {
            array[i++] = c1;
            if (c2 != 0)
                array[where + i++] = c2;
        }
    }

  public void replace(int where, char[] chars, int start, int count)
  {
    System.arraycopy(chars, start, data, where, count);
  }

  public void replace(int where, String string)
  {
    string.getChars(0, string.length(), data, where);
  }

    public boolean equals(Object obj) {
        return obj instanceof FString && equals(this, (FString) obj);
    }

  public int getElementKind()
  {
    return CHAR_VALUE;
  }

  public void consume(Consumer out)
  {
    out.write(data, 0, data.length);
  }

  public void consumePosRange (int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    if (end > size)
      end = size;
    if (end > i)
      out.write(data, i, end - i);
  }

    public FString append(char c) {
        int sz = size;
        if (sz >= data.length)
            ensureBufferLength(sz+1);
        char[] d = data;
        d[sz] = c;
        size = sz + 1;
        return this;
    }

    /** Append a Unicode code point. */
    public FString appendCharacter(int c) {
        int delta;
        if (c < 0x10000)
            delta = 1;
        else if (c == Char.IGNORABLE_CHAR)
            return this;
        else
            delta = 2;
        int sz = size;
        if (sz + delta > data.length)
            ensureBufferLength(sz+delta);
        char[] d = data;
        if (delta > 1) {
            d[sz++] = (char) (((c - 0x10000) >> 10) + 0xD800);
            c = (c & 0x3FF) + 0xDC00;
        }
        d[sz++] = (char) c;
        size = sz;
        return this;
    }

    public FString append(CharSequence csq) {
        if (csq == null)
            csq = "null";
        return append(csq, 0, csq.length());
    }

    public FString append(CharSequence csq, int start, int end) {
        if (csq == null)
            csq = "null";
        int len = end - start;
        int sz = size;
        if (sz+len > data.length)
            ensureBufferLength(sz+len);
        char[] d = data;
        if (csq instanceof String)
            ((String) csq).getChars(start, end, d, sz);
        else if (csq instanceof CharSeq)
            ((CharSeq) csq).getChars(start, end, d, sz);
        else {
            int j = sz;
            for (int i = start; i < end;  i++)
                d[j++] = csq.charAt(i);;
        }
        size = sz+len;
        return this;
    }

    public FString append(Object obj) {
        if (obj instanceof gnu.text.Char)
            appendCharacter(((gnu.text.Char) obj).intValue());
        else if (obj instanceof java.lang.Character)
            appendCharacter(((java.lang.Character) obj).charValue());
        else
            append(obj.toString());
        return this;
    }

  public void writeTo(int start, int count, Appendable dest)
     throws java.io.IOException
  {
    if (dest instanceof java.io.Writer)
      {
        try
          {
            ((java.io.Writer) dest).write(data, start, count);
          }
        catch (java.io.IOException ex)
          {
            throw new RuntimeException(ex);
          }
      }
    else
      {
        dest.append(this, start, start+count);
      }
  }

  public void writeTo(Appendable dest) throws java.io.IOException
  {
    writeTo(0, size, dest);
  }

}
