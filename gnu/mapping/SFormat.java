package gnu.mapping;
import java.io.CharArrayWriter;
import gnu.lists.*;
import gnu.text.Char;

/** A Format to print structured objects on streams.
 * After JDK 1.1 comes out, this should be made compatible with
 * java.text.Format.   We will also encapulate formatting flags
 * and pretty-printing here. */
// FIXME this class is deprecated - use DisplayFormat instead!

public class SFormat // extends gnu.text.ReportFormat
{
  public String format (Object obj)
  {
    CharArrayWriter wr = new CharArrayWriter();
    format(obj, new OutPort(wr));
    return wr.toString();
  }

  public StringBuffer format (Object obj, StringBuffer buffer)
  {
    /* FIXME - more efficient to use a "StringBufferWriter". */
    CharArrayWriter wr = new CharArrayWriter();
    format(obj, new OutPort(wr));
    buffer.append(wr.toCharArray());
    return buffer;
  }

  public void format (Object obj, java.io.PrintWriter ps)
  {
    print (obj, ps);
  }

  public static boolean printReadable(java.io.Writer ps)
  {
    return ps instanceof OutPort && ((OutPort)ps).printReadable;
  }

  public static void print (Object obj, java.io.PrintWriter ps)
  {
    if (obj instanceof Printable)
      ((Printable)obj).print(ps);
    else if (obj instanceof Boolean)
      ps.print(((Boolean)obj).booleanValue() ? "#t" : "#f");
    else if (obj instanceof Char)
      {
	char ch = ((Char) obj).charValue ();
	if (printReadable(ps))
	  ps.print(Char.toScmReadableString(ch));
	else
	  ps.print(ch);
      }
    else if (obj == null)
      ps.print("#!null");
    else if (obj instanceof Object[])
      {
	Object[] arr = (Object[]) obj;
	ps.print ('[');
	for (int i = 0;  i < arr.length;  i++)
	  {
	    if (i > 0)
	      ps.print (' ');
	    print (arr[i], ps);
	  }
	ps.print (']');
      }
    else if (obj instanceof LList)
      {
	ps.print('(');
	boolean first = true;
	while (obj instanceof Pair)
	  {
	    if (! first)
	      ps.print(' ');
	    first = false;
	    Pair pair = (Pair) obj;
	    print(pair.car, ps);
	    obj = pair.cdr;
	  }
	if (obj != LList.Empty)
	  {
	    ps.print(" . ");
	    print(obj, ps);
	  }
	ps.print(')');
      }
    else if (obj instanceof CharSeq)
      {
	if (printReadable(ps))
	  Strings.printQuoted((CharSeq) obj, ps, 0);
	else
	  ps.print(obj);
      }
    else if (obj instanceof SimpleVector)
      {
	SimpleVector vec = (SimpleVector) obj;
	String tag = vec.getTag();
	int size = vec.size;
	ps.print('#');
	if (tag != null)
	  ps.print(tag);
	ps.print('(');
	for (int i = 0;  i < size;  i++)
	  {
	    if (i > 0)
	      ps.print(' ');
	    print(vec.get(i), ps);
	  }
	ps.print(')');
      }
    else if (obj instanceof int[])
      {
	int[] arr = (int[]) obj;
	ps.print ('[');
	for (int i = 0;  i < arr.length;  i++)
	  {
	    if (i > 0)
	      ps.print (' ');
	    ps.print (arr[i]);
	    //ps.print ("#x"+Long.toString((long)arr[i] & 0xffffffffL, 16));
	  }
	ps.print (']');
      }
    else
      ps.print(obj);
  }

}
