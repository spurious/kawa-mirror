package kawa.standard;
import kawa.lang.*;
import gnu.text.*;
import java.text.ParseException;
import java.text.Format;

public class ParseFormat extends Procedure1
{
  boolean emacsStyle = true;

  public ParseFormat (boolean emacsStyle)
  {
    this.emacsStyle = emacsStyle;
  }

  public static final int SEEN_MINUS = 1;
  public static final int SEEN_PLUS  = 2;
  public static final int SEEN_SPACE = 4;
  public static final int SEEN_ZERO  = 8;
  public static final int SEEN_HASH = 16;

  public ReportFormat parseFormat(LineBufferedReader fmt)
    throws java.text.ParseException, java.io.IOException
  {
    StringBuffer fbuf = new StringBuffer(100);
    int position = 0;
    java.util.Vector formats = new java.util.Vector();
    Format format;
    char magic = emacsStyle ? '?' : '~';
    for (;;)
      {
	int ch = fmt.read();
	if (ch >= 0)
	  {
	    if (ch != magic)
	      {
		// FIXME - quote special characters!
		fbuf.append((char) ch);
		continue;
	      }
	    ch = fmt.read();
	    if (ch == magic)
	      {
		fbuf.append((char) ch);
		continue;
	      }
	  }
	int len = fbuf.length();
	if (len > 0)
	  {
	    char[] text = new char[len];
	    fbuf.getChars(0, len, text, 0);
	    fbuf.setLength(0);
	    formats.addElement(new LiteralFormat(text));
	  }
	if (ch < 0)
	  break;
	int digit;
	if (ch == '$')
	  {
	    ch = fmt.read();
	    position = Character.digit((char) ch, 10);
	    if (position < 0)
	      throw new ParseException("missing number (position) after '%$'",
				       -1);
	    for (;;)
	      {
		ch = fmt.read();
		digit = Character.digit((char) ch, 10);
		if (digit < 0)
		  break;
		position = 10 * position + digit;
	      }
	    position--;  /* Convert to zero-origin index. */
	  }

	int flags = 0;
	for (;; ch = fmt.read())
	  {
	    switch ((char) ch)
	      {
	      case '-':  flags |= SEEN_MINUS;  continue;
	      case '+':  flags |= SEEN_PLUS;   continue;
	      case ' ':  flags |= SEEN_SPACE;  continue;
	      case '0':  flags |= SEEN_ZERO;   continue;
	      case '#':  flags |= SEEN_HASH;   continue;
	      }
	    break;
	  }

	int width = -1;
System.err.println("Handle magic");
	digit = Character.digit((char) ch, 10);
	if (digit >= 0)
	  {
	    width = digit;
	    for (;;)
	      {
		ch = fmt.read();
		digit = Character.digit((char) ch, 10);
		if (digit < 0)
		  break;
		width = 10 * width + digit;
	      }
	  }

	int precision = -1;
	if (ch == '.')
	  {
	    precision = 0;
	    for (;;)
	      {
		ch = fmt.read();
		digit = Character.digit((char) ch, 10);
		if (digit < 0)
		  break;
		precision = 10 * precision + digit;
	      }
	  }

	switch (ch)
	  {
	  case 's':
	  case 'S':
	    format = new ObjectFormat(ch == 'S', precision);
	    break;

	  case 'x':
	  case 'X':
	  case 'i':
	  case 'd':
	  case 'o':

	  case 'e':
	  case 'f':
	  case 'g':
	    format = new ObjectFormat(false);  // FIXME
	    break;
	  default:
	    throw new ParseException ("unknown format character '"+ch+"'", -1);
	  }
	if (width > 0)
	  {
	    char padChar = (flags & SEEN_ZERO) != 0 ? '0' : ' ';
	    int where;
	    if ((flags & SEEN_MINUS) != 0)
	      where = 100;
	    else if (padChar == '0')
	      where = -1;
	    else
	      where = 0;
	    format = new gnu.text.PadFormat(format, width, padChar, where);
	  }
	// FIXME handle re-positioning
	//fbuf.append('{');
        // fbuf.append(position);
	//fbuf.append('}');
	formats.addElement(format);
	position++;
      }
    // System.err.println("format: "+fbuf.toString());
    int fcount = formats.size();
System.err.println("fcount:"+fcount);
    if (fcount == 1)
      {
	Object f = formats.elementAt(0);
	if (f instanceof ReportFormat)
	  return (ReportFormat) f;
      }
    Format[] farray = new Format[fcount];
    formats.copyInto(farray);
    return new CompoundFormat(farray);
  }

  public Object apply1 (Object arg)
  {
    try
      {
	if (! emacsStyle)
	  return new LispFormat(arg.toString());
	else
	  {
	    InPort iport;
	    if (arg instanceof FString) 
	      iport = ((FString) arg).open(); 
	    else 
	      iport = new CharArrayInPort(arg.toString()); 
	    try
	      {
		return parseFormat(iport);
	      }
	    finally
	      {
		iport.close();
	      }
	  }
      }
    catch (java.io.IOException ex)
      {
	throw new GenericError("Error parsing format ("+ex+")");
      }
    catch (ParseException ex)
      {
	throw new GenericError("Invalid format ("+ex+")");
      }
    catch (IndexOutOfBoundsException ex)
      {
	throw new GenericError("End while parsing format");
      }
  }
}
