package kawa.lang;
import gnu.bytecode.Method;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.bytecode.Type;
import gnu.mapping.*;
import gnu.expr.*;

/** Representation of fixed-length mutable character strings.
 * Used for the Scheme string type.
 * @author Per Bothner
 */

public class FString extends Sequence implements Printable, Compilable
{

  char[] value;

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
    int length = buffer.length ();
    value = new char[length];
    if (length > 0)
      buffer.getChars (0, length, value, 0);
  }

  public FString copy (int start, int end)
  {
    char[] copy = new char[end-start];
    for (int i = start;  i < end;  i++)
      copy[i-start] = value[i];
    return new FString(copy);
  }

  public FString copy ()
  {
    int i = value.length;
    char[] copy = new char[i];
    while (--i >= 0)
      copy[i] = value[i];
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

  public final Object elementAt (int index)
  {
    return Char.make (value[index]);
  }

  public char charAt (int index)
  {
    if (index < 0 || index >= value.length)
      throw new StringIndexOutOfBoundsException(index);
    return value[index];
  }

  public void setCharAt (int index, char ch)
  {
    if (index < 0 || index >= value.length)
      throw new StringIndexOutOfBoundsException(index);
    value[index] = ch;
  }

  /** Set all the elements to a given character. */
  public final void fill (char ch)
  {
    for (int i = value.length;  --i >= 0; )
      value[i] = ch;
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

  static public ClassType scmStringType;
  static public Method initFStringMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (scmStringType == null)
      {
	scmStringType = ClassType.make("kawa.lang.FString");
	initFStringMethod
	  = scmStringType.addMethod ("<init>", comp.string1Arg,
				      Type.void_type, Access.PUBLIC);
      }
    Literal literal = new Literal (this, scmStringType, comp);
    comp.findLiteral (value);
    return literal;
  }

  public void emit (Literal literal, Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    code.emitNew(scmStringType);
    code.emitDup(scmStringType);
    code.emitPushString(toString ());
    code.emitInvokeSpecial(initFStringMethod);
  }

  /** Change every character to be uppercase. */
  public void makeUpperCase()
  {
    for (int i = value.length;  --i >= 0; )
      value[i] = Character.toUpperCase(value[i]);
  }

  /** Change every character to be lowercase. */
  public void makeLowerCase()
  {
    for (int i = value.length;  --i >= 0; )
      value[i] = Character.toLowerCase(value[i]);
  }

  /** Capitalize this string.
   * Change first character of each word to titlecase,
   * and change teh other characters rest to lowercase. */
  public void makeCapitalize()
  {
    char prev = ' ';
    int len = value.length;
    for (int i = 0;  i < len;  i++)
      {
	char ch = value[i];
	if (! Character.isLetterOrDigit(prev))
	  ch = Character.toTitleCase(ch); 
        else 
          ch = Character.toLowerCase(ch);
	value[i] = ch;
	prev = ch;
      }
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

}
