package gnu.kawa.util;
import gnu.text.Char;
import gnu.mapping.*;

public abstract class AbstractString extends UniformVector implements Printable
{
  /** Kind of position which always stays before text inserted here.
   * An example is the pseudo-mark representing the beginning of the buffer. */
  public static final int BEFORE_MARK_KIND = 0;

  /** Kind of position which stays before or after text inserted here.
   * depending on whether plain insert or insert-before-markers is called.
   * This is how standard Emacs markers behave. */
  public static final int EMACS_MARK_KIND = 1;

  /** Kind of position which always stays after text inserted at the position.
   * An example is the standard behavior of javax.swing.text.Position;
   * another is the pseudo-mark representing the end of the buffer;
   * another is the pseudo-mark representing the Emacs insertion point. */
  public static final int AFTER_MARK_KIND = 2;

  public final Object get (int index)
  {
    if (index < 0 || index >= length())
      throw new StringIndexOutOfBoundsException(index);
    return Char.make(charAt(index));
  }

  /** Copy characters into a destination buffer.
   * Same interface as java.lang.String's getChars. */
  public void getChars (int srcBegin, int srcEnd, char[] dst, int dstBegin)
  {
    for (int i = srcBegin;  i < srcEnd;  i++)
      dst[dstBegin++] = charAt(i);
  }

  /**
   * Write out (part of) this string.
   * @param start index of initial character to write
   * @param count number of characters to write (-1 means continue to end).
   * @param dest where to write the characters
   */
  public void writeTo(int start, int count, java.io.Writer dest)
    throws java.io.IOException
  {
    while (count == -1 || --count >= 0)
      dest.write(charAt(start++));
  }

  public void writeTo(java.io.Writer str) throws java.io.IOException
  {
    writeTo(0, -1, str);
  }

  public abstract char charAt (int index);

  public abstract void setCharAt (int index, char ch);

  public void setElementAt (Object new_value, int index)
  {
    setCharAt(index, ((Char) new_value).charValue());
  }

  public void replace(int where, char[] chars, int start, int count)
  {
    while (--count >= 0)
      setCharAt(where++, chars[start++]);
  }

  public void replace (int where, String string)
  {
    int start = 0;
    for (int count = string.length();  --count >= 0; )
      setCharAt(where++, string.charAt(start++));
  }

  public void replace (int where, AbstractString string)
  {
    int start = 0;
    for (int count = string.length();  --count >= 0; )
      setCharAt(where++, string.charAt(start++));
  }

  public FString copy ()
  {
    int i = length();
    char[] copy = new char[i];
    while (--i >= 0)
      copy[i] = charAt(i);
    return new FString(copy);
  }

  /** Set all the elements to a given character. */
  public void fill (char ch)
  {
    fill(ch, 0, length());
  }

  public void fill (char ch, int start, int count)
  {
    while (--count >= 0)
      setCharAt(start++, ch);
  }

  protected void insert(int where, int count, boolean beforeMarkers)
  {
    // throw new UnsupportedOperationException("insert not supported"); Java 2!
    throw new RuntimeException("insert not supported");
  }

  public void insert(int where, AbstractString str, boolean beforeMarkers)
  {
    int count = str.length();
    insert(where, count, beforeMarkers);
    replace(where, str);
  }

  public void insert(int where, String str, boolean beforeMarkers)
  {
    int count = str.length();
    insert(where, count, beforeMarkers);
    replace(where, str);
  }

  public void insert(int where, char[] chars, int start, int count,
		     boolean beforeMarkers)
  {
    insert(where, count, beforeMarkers);
    replace(where, chars, start, count);
  }

  public void insert(int where, char ch, int count,
		     boolean beforeMarkers)
  {
    insert(where, count, beforeMarkers);
    fill(ch, where, count);
  }

  public final void setAll(Object value)
  {
    fill(((Char) value).charValue());
  }

  /** Change every character to be uppercase. */
  public void makeUpperCase()
  {
    for (int i = length();  --i >= 0; )
      setCharAt(i, Character.toUpperCase(charAt(i)));
  }

  /** Change every character to be lowercase. */
  public void makeLowerCase()
  {
    for (int i = length();  --i >= 0; )
      setCharAt(i, Character.toLowerCase(charAt(i)));
  }

  /** Capitalize this string.
   * Change first character of each word to titlecase,
   * and change the other characters to lowercase. */
  public void makeCapitalize()
  {
    char prev = ' ';
    int len = length();
    for (int i = 0;  i < len;  i++)
      {
	char ch = charAt(i);
	if (! Character.isLetterOrDigit(prev))
	  ch = Character.toTitleCase(ch); 
        else 
          ch = Character.toLowerCase(ch);
	setCharAt(i, ch);
	prev = ch;
      }
  }

  public void print (java.io.PrintWriter ps)
  {
    int len = length();
    if ((ps instanceof OutPort) && ((OutPort)ps).printReadable)
      {
	ps.print ('\"');
	for (int i = 0;  i < len; i++)
	  {
	    char ch = charAt(i);
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
      {
	for (int i = 0;  i < len; i++)
	  {
	    ps.print (charAt(i));
	  }
      }
  }

  /**
   * Make a copy of a substring, returning a String.
   * (Same interface as the Java2 StringBuffer substring method.)
   * (In contract subString makes a shared substring,
   * and takes position values.)
   */
  public String substring(int start, int end)
  {
    char[] chars = new char[end - start];
    getChars(start, end, chars, 0);
    return new String(chars);
  }

  public String toString()
  {
    return substring(0, length());
  }

  /** Make a shared sub-string, using position values.
   * (In contrast, substring makes a copy, and takes raw offsets.)
   * (Does not necessarily do any error checking.)
   */
  public AbstractString subString(int fromPosition, int toPosition)
  {
    return new SubString(this, fromPosition, toPosition);
  }

  public int getStartPosition()
  {
    return createPosition(0, BEFORE_MARK_KIND);
  }

  public int getEndPosition()
  {
    return createPosition(length(), AFTER_MARK_KIND);
  }

  public int createPosition (int offset, int kind)
  {
    return offset;
  }

  public void releasePosition (int position)
  {
  }

  public int getPositionOffset (int position)
  {
    return position;
  }

  public int getPositionKind (int position)
  {
    return 0;
  }

  public String getTag() { return "ch"; }
}
