package kawa.standard;
import java.text.*;
import java.util.*;
import kawa.lang.*;
import java.io.Writer;
import java.io.CharArrayWriter;
import gnu.text.ReportFormat;
import gnu.mapping.*;

public class ObjectFormat extends ReportFormat
{
  /** Maxiumum number of characters to show.
   * Truncate any following characters.
   * The value -1 means "no limit". */
  int maxChars;
  boolean readable;

  private static ObjectFormat readableFormat;
  private static ObjectFormat plainFormat;

  public static ObjectFormat getInstance(boolean readable)
  {
    if (readable)
      {
	if (readableFormat == null)
	  readableFormat = new ObjectFormat(true);
	return readableFormat;
      }
    else
      {
	if (plainFormat == null)
	  plainFormat = new ObjectFormat(false);
	return plainFormat;
      }
  }

  public ObjectFormat(boolean readable)
  {
    this.readable = readable;
    maxChars = -1;
  }

  public ObjectFormat(boolean readable, int maxChars)
  {
    this.readable = readable;
    this.maxChars = maxChars;
  }

  public int format(Object[] args, int start, Writer dst, FieldPosition fpos)
    throws java.io.IOException
  {
    return format(args, start, dst, maxChars, readable);
  }

  /**
   * Return false iff truncation.
   */
  public static boolean format(Object arg, Writer dst,
			       int maxChars, boolean readable)
    throws java.io.IOException
  {
    if (maxChars < 0 && dst instanceof OutPort)
      {
	OutPort oport = (OutPort) dst;
	boolean saveReadable = oport.printReadable;
	try
	  {
	    oport.printReadable = readable;
	    SFormat.print(arg, oport);
	  }
	finally
	  {
	    oport.printReadable = saveReadable;
	  }
	return true;
      }
    else if (maxChars < 0 && dst instanceof CharArrayWriter)
      {
	OutPort oport = new OutPort(dst);
	oport.printReadable = readable;
	SFormat.print(arg, oport);
	oport.flush();
	return true;
      }
    else
      {
	CharArrayWriter wr = new CharArrayWriter();
	OutPort oport = new OutPort(wr);
	oport.printReadable = readable;
	SFormat.print(arg, oport);
	oport.flush();
	int len = wr.size();
	if (maxChars < 0 || len <= maxChars)
	  {
	    wr.writeTo(dst);
	    return true;
	  }
	else
	  {
	    dst.write(wr.toCharArray(), 0, maxChars);
	    return false;
	  }
      }
  }

  public static int format(Object[] args, int start, Writer dst,
			   int maxChars, boolean readable)
    throws java.io.IOException
  {
    format(args[start], dst, maxChars, readable);
    return start + 1;
  }

  public Object parseObject(String text, java.text.ParsePosition status)
  {
    throw new RuntimeException("ObjectFormat.parseObject - not implemented");
  }
}
