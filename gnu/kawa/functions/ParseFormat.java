package gnu.kawa.functions;
import gnu.text.*;
import java.text.ParseException;
import java.text.Format;
import gnu.mapping.*;
import gnu.lists.*;

public class ParseFormat extends Procedure1
{
  public static final ParseFormat parseFormat = new ParseFormat(false);

  boolean emacsStyle = true;
  public static final int PARAM_UNSPECIFIED = LispFormat.PARAM_UNSPECIFIED;
  public static final int PARAM_FROM_LIST = LispFormat.PARAM_FROM_LIST;

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
    return parseFormat(fmt, emacsStyle ? '?' : '~');
  }

  public static ReportFormat parseFormat(LineBufferedReader fmt, char magic)
    throws java.text.ParseException, java.io.IOException
  {
    StringBuffer fbuf = new StringBuffer(100);
    int position = 0;
    java.util.Vector formats = new java.util.Vector();
    Format format;
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

	int width = PARAM_UNSPECIFIED;
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
        else if (ch == '*')
          width = PARAM_FROM_LIST;

	int precision = PARAM_UNSPECIFIED;
	if (ch == '.')
	  {
	    if (ch == '*')
	      precision = PARAM_FROM_LIST;
	    else
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
	    int base;
            int fflags = 0;
	    if (ch == 'd' || ch == 'i')
              base = 10;
	    else if (ch == 'o')
              base = 8;
	    else
              { /* if (ch == 'x' || ch == 'X') */
                base = 16;
                if (ch == 'X') fflags = IntegerFormat.UPPERCASE;
              }
            boolean seenColon = false;
            boolean seenAt = false;
            char padChar
              = (flags & (SEEN_ZERO+SEEN_MINUS)) == SEEN_ZERO ? '0' : ' ';
            if ((flags & SEEN_HASH) != 0)
              fflags |= IntegerFormat.SHOW_BASE;
            if ((flags & SEEN_PLUS) != 0)
              fflags |= IntegerFormat.SHOW_PLUS;
            if ((flags & SEEN_MINUS) != 0)
              fflags |= IntegerFormat.PAD_RIGHT;
            if ((flags & SEEN_SPACE) != 0)
              fflags |= IntegerFormat.SHOW_SPACE;
	    if (precision != PARAM_UNSPECIFIED)
	      {
		flags &= ~ SEEN_ZERO;
		fflags |= IntegerFormat.MIN_DIGITS;
		format = IntegerFormat.getInstance(base, precision,
						   '0', PARAM_UNSPECIFIED,
						   PARAM_UNSPECIFIED, fflags);
	      }
	    else
	      format = IntegerFormat.getInstance(base, width,
						 padChar, PARAM_UNSPECIFIED,
						 PARAM_UNSPECIFIED, fflags);
            break;
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
    return asFormat(arg, emacsStyle ? '?' : '~');
  }

  public static ReportFormat asFormat (Object arg, char style)
  {
    try
      {
	if (arg instanceof ReportFormat)
	  return (ReportFormat) arg;
	if (style == '~')
	  return new LispFormat(arg.toString());
	else
	  {
	    InPort iport;
	    if (arg instanceof FString)
	      {
		FString str = (FString) arg;
		iport = new CharArrayInPort(str.data, str.size);
	      }
	    else 
	      iport = new CharArrayInPort(arg.toString()); 
	    try
	      {
		return parseFormat(iport, style);
	      }
	    finally
	      {
		iport.close();
	      }
	  }
      }
    catch (java.io.IOException ex)
      {
	throw new RuntimeException("Error parsing format ("+ex+")");
      }
    catch (ParseException ex)
      {
	throw new RuntimeException("Invalid format ("+ex+")");
      }
    catch (IndexOutOfBoundsException ex)
      {
	throw new RuntimeException("End while parsing format");
      }
  }
}
