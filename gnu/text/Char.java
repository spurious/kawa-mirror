package gnu.text;
import java.io.*;
import gnu.lists.Consumer;
import gnu.kawa.util.*;

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

  Char (int ch)
  {
    value = ch;
  }

  public void print (Consumer out)
  {
    print(value, out);
  }

  public static void print (int i, Consumer out)
  {
    if (i >= 0x10000)
      {
        out.write((char) (((i - 0x10000) >> 10) + 0xD800));
        out.write((char) ((i & 0x3FF) + 0xDC00));
      }
    else
      out.write((char) i);
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

  static CharMap hashTable = new CharMap();

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
    synchronized (hashTable)
      {
        return hashTable.get(ch);
      }
  }

  public boolean equals (Object obj)
  {
    // This does not work for hashing in make!  Redo make!  FIXME
    // return this == obj;
    return obj != null && (obj instanceof Char)
      && ((Char)obj).intValue() == value;
  }

  static char[] charNameValues = { ' ', '\t', '\n', '\n',
				   '\r', '\f', '\b', '\033',
                                   '\177', '\177', '\177',
                                   '\007', '\007', '\013', '\0' };
  static String[] charNames = { "space",
				"tab",
				"newline",
				"linefeed",
				"return",
				"page",
				"backspace",
				"esc",
				"delete",
				"del",
				"rubout",
				"alarm",
				"bel",
				"vtab",
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
    if (ch < ' ' || ch > 0x7F)
      {
        sbuf.append('x');
        sbuf.append(Integer.toString(ch, 16));
      }
    else
      sbuf.append((char) ch);
    return sbuf.toString();
  }

  /**
   * @serialData Writes the char value as a char.
   *   If the value is {@code > 0xFFFF}, write a pair of surrogate values.
   *   If the value is is a high surrogate only,
   *   write it followed by {@code '\0'}.
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

/** Helper class for mapping Unicode scalar value to Char object. */

class CharMap extends AbstractWeakHashTable<Char,Char>
{
  public Char get (int key)
  {
    cleanup();
    int hash = key;
    int index = hashToIndex(hash);
    for (AbstractWeakHashTable.WEntry<Char,Char> node = table[index];
	 node != null;  node = node.next)
      {
        Char val = node.getValue();
        if (val.intValue() == key)
          return val;
      }
    Char val = new Char(key);
    super.put(val, val);
    return val;
  }

  protected Char getKeyFromValue (Char ch)
  {
    return ch;
  }

  protected boolean matches (Char oldValue, Char newValue)
  {
    return oldValue.intValue() == newValue.intValue();
  }
}
