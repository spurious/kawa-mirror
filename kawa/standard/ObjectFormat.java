package kawa.standard;
import java.text.*;
import java.util.*;
import kawa.lang.*;
import java.io.Writer;
import java.io.CharArrayWriter;
import gnu.text.ReportFormat;

public class ObjectFormat extends ReportFormat
{
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

  public static int format(Object[] args, int start, Writer dst,
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
	    SFormat.print(args[start], oport);
	  }
	finally
	  {
	    oport.printReadable = saveReadable;
	  }
      }
    // FIXME do better if maxChars < 0 && dst instanceof CharArrayWriter
    else
      {
	CharArrayWriter wr = new CharArrayWriter();
	OutPort oport = new OutPort(wr);
	oport.printReadable = readable;
	SFormat.print(args[start], oport);
	char[] chars = wr.toCharArray(); 
	int len = chars.length;
	if (maxChars >= 0 && len > maxChars)
	  len = maxChars;
	dst.write(chars, 0, len);
      }
    return start + 1;
  }

  public Object parseObject(String text, java.text.ParsePosition status)
  {
    throw new GenericError("ObjectFormat.parseObject - not implemented");
  }
}
