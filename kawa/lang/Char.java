package kawa.lang;
import java.io.*;
import java.util.Hashtable;
import gnu.bytecode.ClassType;
import gnu.bytecode.Method;
import gnu.bytecode.Access;
import gnu.bytecode.Type;
import kawa.standard.Scheme;

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

public class Char implements Printable, Compilable
{
  // Leave open the possibility for characters beyond Unicode.
  int value;

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

  public static Char make (char ch)
  {
    return make ((int) ch);
  }

  public boolean equals (Object obj)
  {
    // This does not work for hashing in make!  Redo make!  FIXME
    // return this == obj;
    return obj != null && (obj instanceof Char)
      && ((Char)obj).intValue() == value;
  }

  static char[] charNameValues = { ' ', '\t', '\n', '\n'
				   ,'\r', '\f', '\b', '\177' };
  static String[] charNames = { "space",
				"tab",
				"newline",
				"linefeed",
				"return",
				"page",
				"backspace",
				"rubout" };

  public String toString ()
  {
    StringBuffer buf = new StringBuffer();
    buf.append("[Char '");
    if (value >= (int) ' ' && value < 127)
      buf.append((char) value);
    else
      {
	buf.append('\\');
	buf.append(Integer.toOctalString(value));
      }
    buf.append("']");
    return buf.toString();
  }

  public void print(PrintWriter ps)
  {
    boolean readable = (ps instanceof OutPort)
      && ((OutPort)ps).printReadable;
    char ch = charValue ();
    if (readable)
      {
	ps.print ('#');
	ps.print ('\\');
	for (int i = 0;  i < charNameValues.length;  i++)
	  {
	    if (ch == charNameValues[i])
	      {
		ps.print (charNames[i]);
		return;
	      }
	  }
      }
    ps.print (ch);
  }

  static public ClassType scmCharType;
  public static Method makeCharMethod;
  public static Method charValueMethod;

  public static void initMakeMethods()
  {
    if (scmCharType == null)
      {
	scmCharType = new ClassType ("kawa.lang.Char");
	makeCharMethod = scmCharType.addMethod ("make",
						 Compilation.int1Args,
						 scmCharType,
						 Access.PUBLIC|Access.STATIC);
	charValueMethod = scmCharType.addMethod ("charValue",
						  Type.typeArray0,
						  Scheme.charType,
						  Access.PUBLIC);
      }
  }

  public Literal makeLiteral (Compilation comp)
  {
    initMakeMethods();
    return new Literal (this, scmCharType, comp);
  }

  public void emit (Literal literal, Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    code.emitPushInt(((Char) literal.value).intValue ());
    code.emitInvokeStatic(makeCharMethod);
  }
}
