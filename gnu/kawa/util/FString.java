package gnu.kawa.util;
import java.io.*;
import gnu.mapping.*;

/** Representation of fixed-length mutable character strings.
 * Used for the Scheme string type.
 * @author Per Bothner
 */

public class FString extends AbstractString implements Printable, Externalizable
{
  char[] value;

  public FString ()
  {
  }

  public FString (int num)
  {
    value = new char[num];
  }

  public FString (int num, char ch)
  {
    value = new char[num];
    for (int i = num;  --i >= 0;)
      value[i] = ch;
  }

  /** Create an FString from a char[].
   * Note that this contructor does *not* copy the argument. */
  public FString (char[] values)
  {
    value = values;
  }

  public FString (String str)
  {
    value = str.toCharArray ();
  }

  public FString (StringBuffer buffer)
  {
    this(buffer, 0, buffer.length());
  }

  public FString (StringBuffer buffer, int offset, int length)
  {
    value = new char[length];
    if (length > 0)
      buffer.getChars (offset, offset+length, value, 0);
  }

  public FString (char[] buffer, int offset, int length)
  {
    value = new char[length];
    System.arraycopy(buffer, offset, value, 0, length);
  }

  public FString copy (int start, int end)
  {
    char[] copy = new char[end-start];
    for (int i = start;  i < end;  i++)
      copy[i-start] = value[i];
    return new FString(copy);
  }

  public final int length ()
  {
    return value.length;
  }

  public String toString ()
  {
    return new String (value);
  }

  public char charAt (int index)
  {
    return value[index];
  }

  public void setCharAt (int index, char ch)
  {
    if (index < 0 || index >= value.length)
      throw new StringIndexOutOfBoundsException(index);
    value[index] = ch;
  }

  public void writeTo(int start, int count, java.io.Writer dest)
    throws java.io.IOException
  {
    int len = value.length - start;
    if (count > len || count == -1)
      count = len;
    dest.write(value, start, count);
  }

  /** Set all the elements to a given character. */
  public final void fill (char ch)
  {
    for (int i = value.length;  --i >= 0; )
      value[i] = ch;
  }

  public void replace(int where, char[] chars, int start, int count)
  {
    System.arraycopy(chars, start, value, where, count);
  }

  public void replace(int where, String string)
  {
    string.getChars(0, string.length(), value, where);
  }

  public void getChars (int srcBegin, int srcEnd, char dst[], int dstBegin)
  {
    if (srcBegin < 0 || srcBegin > srcEnd)
      throw new StringIndexOutOfBoundsException(srcBegin);
    if (srcEnd > value.length)
      throw new StringIndexOutOfBoundsException(srcEnd);
    if (dstBegin+srcEnd-srcBegin > dst.length)
      throw new StringIndexOutOfBoundsException(dstBegin);
    if (srcBegin < srcEnd)
      System.arraycopy(value, srcBegin, dst, dstBegin, srcEnd - srcBegin);
  }

  /** Return a char[] contain the characters of this string.
   * It is unspecified if the result is a copy or shares with this FString.
   */
  public char[] toCharArray()
  {
    return value;
  }

  public int hashCode ()
  {
    /* Matches String.hashCode specification, as updated specification in
       http://www.javasoft.com/docs/books/jls/clarify.html. */
    char[] val = value;
    int len = val.length;
    int hash = 0;
    for (int i = 0;  i < len;  i++)
      hash = 31 * hash + val[i];
    return hash;
  }

  public boolean equals (Object obj)
  {
    if (obj == null || !(obj instanceof FString))
      return false;
    char[] str = ((FString) obj).value;
    int n = value.length;
    if (str == null || str.length != n)
      return false;
    for (int i = n;  --i >= 0; )
      {
	if (value[i] != str[i])
	  return false;
      }
    return true;
  }

  public void print (java.io.PrintWriter ps)
  {
    boolean readable = (ps instanceof OutPort)
      && ((OutPort)ps).printReadable;
    int len = length();
    if (readable)
      {
	ps.print ('\"');
	for (int i = 0;  i < len; i++)
	  {
	    char ch = value[i];
	    if ((ch == '\\' || ch == '\"'))
	      ps.print ('\\');
	    /*
	    // These escapes are not standard Scheme,
	    // so should probably not be enabled by default.
	    else if (ch == '\n')
	      { ps.print("\\n"); continue; }
	    else if (ch == '\r')
	      { ps.print("\\r"); continue; }
	    else if (ch == '\t')
	      { ps.print("\\t"); continue; }
	    */
	    ps.print (ch);
	  }
	ps.print ('\"');
      }
    else
      ps.print(value);
  }

  /** Return a new InPort that reads characters from this string. */
  public InPort open ()
  { return new CharArrayInPort(value, value.length); }

  /**
   * @serialData Write the length (using writeInt), followed by
   *   the elements in order (written using writeChar).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int len = value.length;
    out.writeInt(len);
    for (int i = 0;  i < len;  i++)
      out.writeChar(value[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int len = in.readInt();
    char[] value = new char[len];
    for (int i = 0;  i < len;  i++)
      value[i] = in.readChar();
    this.value = value;
  }
}
