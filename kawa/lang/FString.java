package kawa.lang;
import gnu.bytecode.Method;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.bytecode.Type;

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
	scmStringType = new ClassType ("kawa.lang.FString");
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
    comp.method.compile_new (scmStringType);
    comp.method.compile_dup (scmStringType);
    comp.method.compile_push_string (toString ());
    comp.method.compile_invoke_special (initFStringMethod);
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
	    ps.print (ch);
	  }
	ps.print ('\"');
      }
    else
      ps.print(value);
  }
}
