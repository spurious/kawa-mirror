package gnu.lists;

import gnu.text.Char;
import java.io.*;
import java.util.*;

public class SubCharSeq extends AbstractSequence<Char>
    implements CharSeq, RandomAccess, Externalizable
{
    CharSequence base;
    int start;
    int end;

    public SubCharSeq(CharSequence base, int start, int end) {
        this.base = base;
        this.start = start;
        this.end = end;
    }

  /** Get length of string, in characters.
   * Synonym for size(), for compatibility with String and StringBuffer. */
  public int length()
  {
      return end - start;
  }

    public char charAt(int index) {
        if (index < 0 || index >= end - start)
            throw new IndexOutOfBoundsException();
        return base.charAt(start + index);
    }

    public Char get(int index) {
        return Char.valueOf(Strings.characterAt(base, start, end,
                                                index + start));
    }

    public int size() {
        return length();
    }

  /** Copy characters into a destination buffer.
   * Same interface as java.lang.String's getChars. */
  public void getChars (int srcBegin, int srcEnd, char[] dst, int dstBegin)
  {
    for (int i = srcBegin;  i < srcEnd;  i++)
      dst[dstBegin++] = charAt(i);
  }

    public void setCharAt(int index, char ch) {
        if (index < 0 || index >= size())
            throw new IndexOutOfBoundsException();
        ((CharSeq) base).setCharAt(start + index, ch);
    }

    public void setCharacterAt(int index, int ch) {
        if (index < 0 || index >= size())
            throw new IndexOutOfBoundsException();
        ((CharSeq) base).setCharacterAt(start + index, ch);
    }

    /** Set all the elements to a given character. */
    public void fill(char value) {
        ((CharSeq) base).fill(start, end, value);
    }

    public void fill(int fromIndex, int toIndex, char value) {
        if (fromIndex < 0 || toIndex < fromIndex || start + toIndex > end)
            throw new IndexOutOfBoundsException();
        ((CharSeq) base).fill(start + fromIndex, start + toIndex, value);
    }

  public void writeTo(int start, int count, Appendable dest)
    throws java.io.IOException
  {
    if (start < 0 || count < 0 || start + count > length())
      throw new IndexOutOfBoundsException();
    dest.append(base, this.start+start, count);
  }

    public void writeTo(Appendable dest)
        throws java.io.IOException {
        dest.append(base, start, end);
    }

  public String toString()
  {
    int sz = size();
    StringBuffer sbuf = new StringBuffer(sz);
    for (int i = 0;  i < sz;  i++)
      sbuf.append(charAt(i));
    return sbuf.toString();
  }

    private SubCharSeq subCharSeq(int start, int end) {
        int sz = size();
        if (start < 0 || end < start || end > sz)
            throw new IndexOutOfBoundsException();
        return new SubCharSeq(base,
                               this.start + start,
                               this.start + end);
    }

    public java.util.List subList(int fromIx, int toIx) {
        return subCharSeq(fromIx, toIx);
    }

    public CharSeq subSequence(int start, int end) {
        return subCharSeq(start, end);
    }

    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeObject(base);
        out.writeInt(start);
        out.writeInt(end);
    }
    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        base = (CharSequence) in.readObject();
        start = in.readInt();
        end = in.readInt();
    }


}
