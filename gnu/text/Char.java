package gnu.text;
import java.io.*;
import java.util.Hashtable;

/**
 * A wrapper for characters.
 * #author	Per Bothner
 */

/*
 * This is similar to java.lang.Character, so why don't we just use that?
 * Good question, since this new class makes us a little less compatible
 * with "standard" Java.  However, that should be fairly minor, since
 * few methods will require Character parameters or arrays (better to
 * just use chars then).
 * The Char class uses hashing to ensure that characters are unique.
 * Thus equal? Char are eq?, which is convenient.
 * Also, using our own class lets us make sure it implements Printable.
 * Finally, we can use 32-bit character values to allow for non-Unicode chars.
 */

public class Char
  implements
  /* #ifdef JAVA2 */
  Comparable,
  /* #endif */
  Externalizable
{
  // Leave open the possibility for characters beyond Unicode.
  int value;

  /** Should only be used for serialization. */
  public Char ()
  {
  }

  private Char (char ch)
  {
    value = (int) ch;
  }

  private Char (int ch)
  {
    value = ch;
  }

  public final char charValue ()
  {
    return (char) value;
  }

  public final int intValue ()
  {
    return value;
  }

  public int hashCode ()
  {
    return value;
  }

  static Char[] ascii;

  static Char temp = new Char (0);
  static Hashtable hashTable;

  static
  {
    ascii = new Char[128];
    for (int i = 128; --i >= 0; )
      ascii[i] = new Char(i);
  }

  public static Char make (int ch)
  {
    if (ch < 128)
      return ascii[ch];
    else
      {
	// Re-writing this will allow equals to just use ==.  FIXME.
	temp.value = ch;
	if (hashTable == null)
	  hashTable = new Hashtable ();
	Object entry = hashTable.get (temp);
	if (entry != null)
	  return (Char) entry;
	Char newChar = new Char (ch);
	hashTable.put (newChar, newChar);
	return newChar;
      }
  }

  public boolean equals (Object obj)
  {
    // This does not work for hashing in make!  Redo make!  FIXME
    // return this == obj;
    return obj != null && (obj instanceof Char)
      && ((Char)obj).intValue() == value;
  }

  static char[] charNameValues = { ' ', '\t', '\n', '\n'
				   ,'\r', '\f', '\b',
				   '\033', '\177', '\177', '\007', '\0' };
  static String[] charNames = { "space",
				"tab",
				"newline",
				"linefeed",
				"return",
				"page",
				"backspace",
				"esc",
				"del",
				"rubout",
				"bel",
				"nul"};

  public static int nameToChar(String name)
  {
    for (int i = charNames.length; --i >= 0 ; )
      {
        if (charNames[i].equals(name))
          return charNameValues[i];
      }
    for (int i = charNames.length; --i >= 0 ; )
      {
        if (charNames[i].equalsIgnoreCase(name))
          return charNameValues[i];
      }
    int len = name.length();
    if (len > 1 && name.charAt(0) == 'u')
      {
	int value = 0;
	for (int pos = 1;  ;  pos++)
	  {
	    if (pos == len)
	      return value;
	    int dig = Character.digit(name.charAt(pos), 16);
	    if (dig < 0)
	      break;
	    value = (value << 4) + dig;
	  }
      }

    // Check for Emacs control character syntax.
    if (len == 3 && name.charAt(1) == '-')
      {
	char ch = name.charAt(0);
	if (ch == 'c' || ch == 'C')
	  {
	    ch = name.charAt(2);
	    return ch & 31;
	  }
      }

    return -1;
  }

  public String toString ()
  {
    StringBuffer buf = new StringBuffer();
    buf.append('\'');
    if (value >= (int) ' ' && value < 127 && value != '\'')
      buf.append((char) value);
    else
      {
	buf.append('\\');
	if (value == '\'')
	  buf.append('\'');
	else if (value == '\n')
	  buf.append('n');
	else if (value == '\r')
	  buf.append('r');
	else if (value == '\t')
	  buf.append('t');
	else if (value < 256)
	  {
	    String str = Integer.toOctalString(value);
	    for (int i = 3 - str.length(); --i >= 0; )
	      buf.append('0');
	    buf.append(str);
	  }
	else
	  {
	    buf.append('u');
	    String str = Integer.toHexString(value);
	    for (int i = 4 - str.length(); --i >= 0; )
	      buf.append('0');
	    buf.append(str);
	  }
      }
    buf.append('\'');
    return buf.toString();
  }

  public static String toScmReadableString (int ch)
  {
    StringBuffer sbuf = new StringBuffer(20);
    sbuf.append("#\\");
    for (int i = 0;  i < charNameValues.length;  i++)
      {
	if ((char) ch == charNameValues[i])
	  {
	    sbuf.append(charNames[i]);
	    return sbuf.toString();
	  }
      }
    if (ch < 8)
      {
	sbuf.append('0');  // make sure there at least two octal digits
	sbuf.append(ch);
      }
    else if (ch < ' ' || ch > 0x7F)
      {
	sbuf.append(Integer.toString(ch, 8));
      }
    else
      sbuf.append((char) ch);
    return sbuf.toString();
  }

  /**
   * @serialData Writes the char value as a char.
   *   If the value is > 0xFFFF, write a pair of surrogate values.
   *   If the value is is a high surrogate only, write it followed by '\0'.
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    if (value > 0xD800)
      {
	if (value > 0xFFFF)
	  {
	    out.writeChar(((value - 0x10000) >> 10) + 0xD800);
	    value = (value & 0x3FF) + 0xDC00;
	  }
	else if (value <= 0xDBFF)
	  {
	    out.writeChar(value);
	    value = '\0';
	  }
      }
    out.writeChar(value);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    value = in.readChar();
    if (value >= 0xD800 && value < 0xDBFF)
      {
	char next = in.readChar();
	if (next >= 0xDC00 && next <= 0xDFFF)
	  value = ((value - 0xD800) << 10) + (next - 0xDC00) + 0x10000;
      }
  }

  public Object readResolve() throws ObjectStreamException
  {
    return make(value);
  }

  public int compareTo(Object o)
  {
    return value - ((Char) o).value;
  }
}
