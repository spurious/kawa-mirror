package kawa.lang;
import java.io.CharArrayWriter;

/** A Format to print structured objects on streams.
 * After JDK 1.1 comes out, this should be made compatible with
 * java.text.Format.   We will also encapulate formatting flags
 * and pretty-printing here. */

public class SFormat // extends java.text.Format  [in JDK 1.1.]
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

  public static void print (Object obj, java.io.PrintWriter ps)
  {
    if (obj instanceof kawa.lang.Printable)
      ((kawa.lang.Printable)obj).print(ps);
    else if (obj instanceof Boolean)
      ps.print(((Boolean)obj).booleanValue() ? "#t" : "#f");
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
      ps.print(obj.toString ());
  }

}
