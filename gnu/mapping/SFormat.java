package gnu.mapping;
import java.io.CharArrayWriter;
import gnu.lists.*;
import gnu.text.Char;

/** A Format to print structured objects on streams.
 * After JDK 1.1 comes out, this should be made compatible with
 * java.text.Format.   We will also encapulate formatting flags
 * and pretty-printing here. */
// FIXME this class is deprecated - use DisplayFormat instead!

public class SFormat
{
  private SFormat() { }

  public static void print (Object obj, java.io.PrintWriter ps)
  {
    if (ps instanceof OutPort && ((OutPort)ps).printReadable)
      kawa.standard.Scheme.writeFormat.writeObject(obj, ps);
    else
      kawa.standard.Scheme.displayFormat.writeObject(obj, ps);
  }

}
