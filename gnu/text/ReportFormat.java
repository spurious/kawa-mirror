package gnu.text;
import java.text.Format;
import java.text.FieldPosition;
import java.io.Writer;
import java.io.CharArrayWriter;

public abstract class ReportFormat extends Format
{
  public static int result(int resultCode, int nextArg)
  {
    return (resultCode << 24) | nextArg;
  }
  public static int nextArg(int result) { return result & 0xffffff; }
  public static int resultCode(int result) { return result >>> 24; }

  /** Format an array of arguments, and write out the result.
   * @param dst where to write the result
   * @param args the objets to be formatted
   * @param start the index (in args) of the argument to start with
   * @return an integer result(resultCode, nextArg), where
   * nextArg is the index following the last argument processed, and
   * code is a result code (normally 0, or negative if early termintation
   */
  public abstract int format(Object[] args, int start,
			     Writer dst, FieldPosition fpos)
    throws java.io.IOException;

  public StringBuffer format(Object obj, StringBuffer sbuf, FieldPosition fpos)
  {
    format((Object[]) obj, 0, sbuf, fpos);
    return sbuf;
  }

  public int format(Object[] args, int start,
		    StringBuffer sbuf, FieldPosition fpos)
  {
    CharArrayWriter wr = new CharArrayWriter();
    try
      {
	start = format(args, start, wr, fpos);
	if (start < 0)
	  return start;
      }
    catch (java.io.IOException ex)
      {
	throw new Error("unexpected exception: "+ex);
      }
    sbuf.append(wr.toCharArray());
    return start;
  }

  public static int format(Format fmt, Object[] args, int start, 
			   Writer dst, FieldPosition fpos) 
    throws java.io.IOException
  {
    if (fmt instanceof ReportFormat)
      return ((ReportFormat) fmt).format(args, start, dst, fpos);
    StringBuffer sbuf = new StringBuffer();
    if (fmt instanceof java.text.MessageFormat)
      start = format(fmt, args, start, sbuf, fpos);
    else
      fmt.format(args[start++], sbuf, fpos);
    int slen = sbuf.length();
    char[] cbuf = new char[slen];
    sbuf.getChars(0, slen, cbuf, 0);
    dst.write(cbuf);
    return start;
  }

  public static int format(Format fmt, Object[] args, int start, 
			   StringBuffer sbuf, FieldPosition fpos) 
  {
    if (fmt instanceof ReportFormat)
      return ((ReportFormat) fmt).format(args, start, sbuf, fpos);
    int nargs;
    Object arg;
    if (fmt instanceof java.text.MessageFormat)
      {
	nargs = args.length - start;
	if (start > 0)
	  {
	    Object[] subarr = new Object[args.length - start];
	    System.arraycopy(args, start, subarr, 0, subarr.length);
	    arg = subarr;
	  }
	else
	  arg = args;
      }
    else
      {
	arg = args[start];
	nargs = 1;
      }
    fmt.format(arg, sbuf, fpos);
    return start + nargs;
  }

  public static void print (Writer dst, String str)
    throws java.io.IOException
  {
    if (dst instanceof java.io.PrintWriter)
      ((java.io.PrintWriter) dst).print(str);
    else
      dst.write(str.toCharArray());
  }

  public Object parseObject(String text, java.text.ParsePosition status)
  {
    throw new Error("ReportdFormat.parseObject - not implemented");
  }

}
