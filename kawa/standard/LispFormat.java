package kawa.standard;
import gnu.text.*;
import java.text.FieldPosition;
import java.text.Format;
import java.text.ParseException;
import java.io.Writer;
import gnu.math.*;
import kawa.lang.*;

/** A representation of a parsed Common Lisp-style format. */

public class LispFormat extends CompoundFormat
{
  public static final int SEEN_COLON = 0x4000000;
  public static final int SEEN_AT = 0x2000000;

  public static final int PARAM_FROM_LIST = 0x8003000;
  public static final int PARAM_FROM_COUNT = 0x8001000;
  public static final int PARAM_UNSPECIFIED = 0x8004000;
  public static final String paramFromList = "<from list>";
  public static final String paramFromCount = "<from count>";
  public static final String paramUnspecified = "<unspecified>";

  public LispFormat(char[] format, int offset, int length)
    throws ParseException
  {
    super(null, 0);
    // The index in spec of the most recent ~{, ~(, ~{ or ~[.
    int start_nesting = -1;
    int choices_seen = 0;  // Number of "~;" seen.

    StringBuffer litbuf = new StringBuffer(100);
    java.util.Stack stack = new java.util.Stack();

    int limit = offset + length;
    int i = offset;
    for (;;)
      {
	if ((i >= limit || format[i] == '~') && litbuf.length() > 0)
	  {
	    stack.push(new LiteralFormat(litbuf));
	    litbuf.setLength(0);
	  }
	if (i >= limit)
	  break;
	char ch = format[i++];
	if (ch != '~')
	  {
	    litbuf.append(ch);
	    continue;
	  }
	int speci = stack.size();
	ch = format[i++];
	for (;;)
	  {
	    if (ch == '#')
	      {
		stack.push(paramFromCount);
		ch = format[i++];
	      }
	    else if (ch == 'v' || ch == 'V')
	      {
		stack.push(paramFromList);
		ch = format[i++];
	      }
	    else if (ch == '-' || Character.digit(ch, 10) >= 0)
	      {
		boolean neg = (ch == '-');
		if (neg)
		  ch = format[i++];
		int val = 0;
		int start = i;
		for (;;)
		  {
		    int dig = Character.digit(ch, 10);
		    if (dig < 0)
		      break;
		    val = 10 * val + dig;
		    ch = format[i++];
		  }
		stack.push(i - start < 8 ? IntNum.make(neg ? - val : val)
			   : IntNum.valueOf(format, start, i-start, 10, neg));
	      }
	    else if (ch == '\'')
	      {
		stack.push(Char.make(format[i++]));
		ch = format[i++];
	      }
	    else if (ch == ',')
	      {
		stack.push(paramUnspecified);
	      }
	    else
	      break;
	    if (ch != ',')
	      break;
	    ch = format[i++];
	  }
	int flags = 0;
	for (;;)
	  {
	    if (ch == ':')
	      flags |= SEEN_COLON;
	    else if (ch == '@')
	      flags |= SEEN_AT;
	    else
	      break;
	    ch = format[i++];
	  }
	ch = Character.toUpperCase(ch);
	boolean seenColon = (flags & SEEN_COLON) != 0;
	boolean seenAt = (flags & SEEN_AT) != 0;
	int numParams = stack.size() - speci;
	Format fmt;
	int minWidth, padChar, charVal, param1, param2, param3;
	switch (ch)
	  {
	  case 'R':  case 'D':  case 'O':  case 'B':  case 'X':
	    int argstart = speci;
	    int base;
	    if (ch == 'R')  base = getParam(stack, argstart++);
	    else if (ch == 'D')  base = 10;
	    else if (ch == 'O')  base = 8;
	    else if (ch == 'X')  base = 16;
	    else base = 2;
	    minWidth = getParam(stack, argstart);
	    padChar = getParam(stack, argstart+1);
	    int commaChar = getParam(stack, argstart+2);
	    int commaInterval = getParam(stack, argstart+3);
	    fmt = LispIntegerFormat.getInstance(base, minWidth,
						padChar, commaChar,
						commaInterval,
						seenColon, seenAt);
	    break;
	  case 'P':
	    fmt = LispPluralFormat.getInstance(seenColon, seenAt);
	    break;
	  case 'F':
	    LispRealFormat rfmt = new LispRealFormat();
	    rfmt.width = getParam(stack, speci);
	    rfmt.precision = getParam(stack, speci+1);
	    rfmt.scale = getParam(stack, speci+2);
	    rfmt.overflowChar = getParam(stack, speci+3);
	    rfmt.padChar = getParam(stack, speci+4);
	    rfmt.showPlus = seenAt;
	    fmt = rfmt;
	    break;
	  case 'E':  case 'G':  case '$':  // FIXME
	  case 'A':  case 'S':
	    fmt = ObjectFormat.getInstance(ch == 'S');
	    if (numParams > 0)
	      {
		minWidth = getParam(stack, speci);
		int colInc = getParam(stack, speci+1);
		int minPad = getParam(stack, speci+2);
		padChar = getParam(stack, speci+3);
		fmt = new LispObjectFormat((ReportFormat) fmt,
					   minWidth, colInc, minPad,
					   padChar, seenAt ? 0 : 100);
	      }
	    break;
	  case 'C':
	    charVal = numParams > 0 ? getParam(stack, speci)
	      : PARAM_FROM_LIST;
	    fmt = LispCharacterFormat.getInstance(charVal, 1,
						  seenAt, seenColon);
	    break;
	  case '*':
	    fmt = new LispRepositionFormat(getParam(stack, speci),
					   seenColon, seenAt);
	    break;
	  case '(':
	    ch = seenColon ? (seenAt ? 'U' : 'C') : (seenAt ? 'T': 'L');
	    CaseConvertFormat cfmt = new CaseConvertFormat(null, ch);
	    stack.setSize(speci);
	    stack.push(cfmt);
	    stack.push(IntNum.make(start_nesting));
	    start_nesting = speci;
	    continue;
	  case ')':
	    if (start_nesting < 0
		|| ! (stack.elementAt(start_nesting)
		      instanceof CaseConvertFormat))
	      throw new ParseException("saw ~) without matching ~(", i);
	    cfmt = (CaseConvertFormat) stack.elementAt(start_nesting);
	    cfmt.setBaseFormat(popFormats(stack, start_nesting + 2, speci));
	    start_nesting = ((IntNum) stack.pop()).intValue();
	    continue;
	  case '?':
	    LispIterationFormat lfmt = new LispIterationFormat();
	    lfmt.seenAt = seenAt;
	    lfmt.maxIterations = 1;
	    lfmt.atLeastOnce = true;
	    fmt = lfmt;
	    break;
	  case '{':
	    lfmt = new LispIterationFormat();
	    lfmt.seenAt = seenAt;
	    lfmt.seenColon = seenColon;
	    lfmt.maxIterations = getParam(stack, speci);
	    stack.setSize(speci);
	    stack.push(lfmt);
	    stack.push(IntNum.make(start_nesting));
	    start_nesting = speci;
	    continue;
	  case '}':
	    if (start_nesting < 0
		|| ! (stack.elementAt(start_nesting)
		      instanceof LispIterationFormat))
	      throw new ParseException("saw ~} without matching ~{", i);
	    lfmt = (LispIterationFormat) stack.elementAt(start_nesting);
	    lfmt.atLeastOnce = seenColon;
	    if (speci > start_nesting + 2)
	      lfmt.body = popFormats(stack, start_nesting + 2, speci);
	    start_nesting = ((IntNum) stack.pop()).intValue();
	    continue;
	  case '[':
	    LispChoiceFormat afmt = new LispChoiceFormat();
	    afmt.param = getParam(stack, speci);
	    if (afmt.param == PARAM_UNSPECIFIED)
	      afmt.param = PARAM_FROM_LIST;
	    if (seenColon)
	      afmt.testBoolean = true;
	    if (seenAt)
	      afmt.skipIfFalse = true;
	    stack.setSize(speci);
	    stack.push(afmt);
	    stack.push(IntNum.make(start_nesting));
	    stack.push(IntNum.make(choices_seen));
	    start_nesting = speci;
	    choices_seen = 0;
	    continue;
	  case ';':
	    if (start_nesting >= 0)
	      {
		if (stack.elementAt(start_nesting)
		    instanceof LispChoiceFormat)
		  {
		    afmt = (LispChoiceFormat) stack.elementAt(start_nesting);
		    if (seenColon)
		      afmt.lastIsDefault = true;
		    fmt = popFormats(stack,
				     start_nesting + 3 + choices_seen, speci);
		    stack.push(fmt);
		    choices_seen++;
		    continue;
		  }
		// else if saw ~< ...
	      }
	    throw new ParseException("saw ~; without matching ~[ or ~<", i);
	  case ']':
	    if (start_nesting < 0
		|| ! (stack.elementAt(start_nesting)
                      instanceof LispChoiceFormat))
              throw new ParseException("saw ~] without matching ~[", i);
	    fmt = popFormats(stack, start_nesting + 3 + choices_seen, speci);
	    stack.push(fmt);
	    afmt = (LispChoiceFormat) stack.elementAt(start_nesting);
	    afmt.choices = getFormats(stack, start_nesting + 3, stack.size());
	    stack.setSize(start_nesting + 3);
	    choices_seen = ((IntNum) stack.pop()).intValue();
	    start_nesting = ((IntNum) stack.pop()).intValue();
	    continue;
	  case '^':
	    param1 = getParam(stack, speci);
	    param2 = getParam(stack, speci+1);
	    param3 = getParam(stack, speci+2);
	    fmt = new LispEscapeFormat(param1, param2, param3);
	    break;
	  case '\n':
	    if (seenAt)
	      litbuf.append(ch);
	    if (! seenColon)
	      {
		for (;;)
		  {
		    ch = format[i++];
		    if (! Character.isWhitespace(ch))
		      break;
		  }
		i--;
	      }
	    continue;
	  case '!':
	    fmt = FlushFormat.getInstance();
	    break;
	  case 'T':
	    param1 = getParam(stack, speci);
	    param2 = getParam(stack, speci+1);
	    fmt = new LispTabulateFormat(param1, param2, seenAt);
	    break;
	  case '~':
	    if (numParams == 0)
	      {
		litbuf.append(ch);
		continue;
	      }
	    /* ... otherwise fall through ... */
	  case '&':  // should actually do (fresh-line) FIXME
	  case '|':
	  case '%':
	    int count = getParam(stack, speci);
	    if (count == PARAM_UNSPECIFIED)
	      count = 1;
	    // EXTENSION:  Allow repeating other characters than '~'.
	    charVal = getParam(stack, speci+1);
	    if (charVal == PARAM_UNSPECIFIED) 
	      charVal = ch == '|' ? '\f' : ch == '%' || ch == '&' ? '\n' : '~';
	    fmt = LispCharacterFormat.getInstance(charVal, count,
						  false, false);
	    break;
          default:
	    throw new ParseException("unrecognized format specifier ~"+ch, i);
          }
	stack.setSize(speci);
	stack.push(fmt);
      }
    if (i > limit)
      throw new IndexOutOfBoundsException();
    if (start_nesting >= 0)
      {
	throw new ParseException("missing ~] or ~}", i);
      }
    this.length = stack.size();
    this.formats = new Format[this.length];
    stack.copyInto(this.formats);
  }

  static Format[] getFormats(java.util.Vector vector, int start, int end)
  {
    Format[] f = new Format[end - start];
    for (int i = start;  i < end;  i++)
      f[i - start] = (Format) vector.elementAt(i);
    return f;
  }

  static Format popFormats(java.util.Vector vector, int start, int end)
  {
    Format f;
    if (end == start + 1)
      f = (Format) vector.elementAt(start);
    else
      f = new CompoundFormat(getFormats(vector, start, end));
    vector.setSize(start);
    return f;
  }

  public LispFormat (String str)
    throws ParseException
  {
    this(str.toCharArray());
  }

  /*
  private void clearSpecs (int speci, int max)
  {
    int num = specs_length - speci - 1;
    for (int i = num;  i < max;  i++)
      addSpec(PARAM_UNSPECIFIED);
    specs_length = speci + 1;
  }
  */

  /*
  private void addSpec(Format fmt)
  {
    if (formats == null)
      formats = new Format[4];
    else
      {
	if (this.length == formats.length)
	  {
	    Format[] newformats = new Format[2 * this.length];
	    System.arraycopy(formats, 0, newformats, 0, this.length);
	    formats = newformats;
	  }
      }
    formats[this.length] = fmt;
    addSpec(this.length);
    this.length++;
  }
  */

  /*
  private void addSpec(int val)
  {
    //System.err.println("addSpec("+val+") at:"+specs_length);
    int old_size = specs.length;
    if (specs_length >= old_size)
      {
	int[] new_specs = new int[2 * old_size];
	System.arraycopy(specs, 0, new_specs, 0, old_size);
	specs = new_specs;
      }
    specs[specs_length++] = val;
  }
  */

  public LispFormat(char[] format)
    throws ParseException
  {
    this(format, 0, format.length);
  }

  public static int getParam(Object arg, int defaultValue)
  {
    if (arg instanceof Number)
      return ((Number) arg).intValue();
    if (arg instanceof Character)
      return ((Character) arg).charValue();
    if (arg instanceof kawa.lang.Char)
      return ((kawa.lang.Char) arg).charValue();
    //if (arg == null || arg == Boolean.FALSE || arg == Special.dfault)
    return defaultValue;
  }

  public static int getParam(java.util.Vector vec, int index)
  {
    if (index >= vec.size())
      return PARAM_UNSPECIFIED;
    Object arg = vec.elementAt(index);
    if (arg == paramFromList)
      return PARAM_FROM_LIST;
    if (arg == paramFromCount)
      return PARAM_FROM_COUNT;
    if (arg == paramUnspecified)
      return PARAM_UNSPECIFIED;
    return getParam(arg, PARAM_UNSPECIFIED);
  }

  static int getParam(int param, int defaultValue, Object[] args, int start)
  {
    if (param == PARAM_FROM_COUNT)
      return args.length - start;
    if (param == PARAM_FROM_LIST)
      return getParam(args[start], defaultValue);
    if (param == PARAM_UNSPECIFIED)
      return defaultValue;
    // Need to mask off flags etc?
    return param;
  }

  static char getParam(int param, char defaultValue, Object[] args, int start)
  {
    return (char) getParam (param, (int) defaultValue, args, start);
  }

  /** Get the index'th parameter for the conversion specification specs[speci].
   * Note that parameters are numbered from 1 to numParams(speci).
   * The list of arguments to be converted is args, with the current index
   * (as of the start of this conversion, i.e. not taking into account
   * earlier PARAM_FROM_LIST paramaters for thsi conversion) in start.
   * The default value (used if PARAM_UNSPECIFIED) is defaultValue.
   */
  /*
  int getParam(int speci, int index, int defaultValue, Object[] args, int start)
  {
    int num_params = numParams(speci);
    int param = index <= num_params ? specs[speci+index] : PARAM_UNSPECIFIED;
    if (param == PARAM_FROM_LIST || param == PARAM_FROM_COUNT)
      start += adjustArgsStart(speci, index);
    return getParam(param, defaultValue, args, start);
  }
  */

  static IntNum asInteger(Object arg)
  {
    try
      {
	if (arg instanceof RealNum)
	  return ((RealNum) arg).toExactInt(Numeric.ROUND);
	if (arg instanceof Long)
	  return IntNum.make(((Long) arg).longValue());
	if (arg instanceof Number)
	  return RealNum.toExactInt(((Number) arg).doubleValue(),
				    Numeric.ROUND);
      }
    catch (Exception ex)
      {
      }
    return null;
  }

  /** Convert sequence (or Object[]) to Object[].
   * Return null if not a valid Sequence. */
  public static Object[] asArray (Object arg)
  {
    if (arg instanceof Object[])
      return (Object[]) arg;
    if (!(arg instanceof Sequence))
      return null;
    int count = ((Sequence) arg).length();
    Object[] arr = new Object[count];
    int i = 0;
    while (arg instanceof Pair)
      {
	Pair pair = (Pair) arg;
	arr[i++] = pair.car;
	arg = pair.cdr;
      }
    if (i < count)
      {
	if (! (arg instanceof Sequence))
	  return null;
	int npairs = i;
	Sequence seq = (Sequence) arg;
	for (; i < count; i++)
	  arr[i] = seq.elementAt(npairs + i);
      }
    return arr;
  }
}

/** Add plural suffixes ("s" or "y/ies") of English words.
 * Used to implement the Common Lisp ~P ('Plural') format operator. */

class LispPluralFormat extends ReportFormat
{
  boolean backup;
  boolean y;

  public static LispPluralFormat getInstance (boolean backup, boolean y)
  {
    LispPluralFormat fmt = new LispPluralFormat();
    fmt.backup = backup;
    fmt.y = y;
    return fmt;
  }

  public int format(Object[] args, int start, 
		    Writer dst, FieldPosition fpos) 
    throws java.io.IOException
  {
    if (backup)
      start--;
    Object arg = args[start++];
    boolean plural = arg != IntNum.one();
    if (y)
      print(dst, plural ? "ies" : "y");
    else if (plural)
      dst.write('s');
    return start;
  }
}

/** Handle formatting of integers.
 * Used to implement the Common Lisp ~D (Decimal), ~X (Hexadecimal),
 * ~O (Octal), ~B (Binary), and ~R (Radix) Common Lisp formats operators. */

class LispIntegerFormat extends ReportFormat
{
  int base;
  int minWidth;
  int padChar;
  int commaChar;
  int commaInterval;
  boolean printCommas;
  boolean printPlus;

  private static LispIntegerFormat plainDecimalFormat;

  public static LispIntegerFormat getInstance()
  {
    if (plainDecimalFormat == null)
      plainDecimalFormat = new LispIntegerFormat();
    return plainDecimalFormat;
  }

  public LispIntegerFormat ()
  {
    base = 10;
    minWidth = 1;
    padChar = (int) ' ';
    commaChar = (int) ',';
    commaInterval = 3;
    printCommas = false;
    printPlus = false;
  }

  public static Format
  getInstance (int base, int minWidth, int padChar,
	       int commaChar, int commaInterval,
	       boolean printCommas, boolean printPlus)
  {
    if (base == LispFormat.PARAM_UNSPECIFIED)
      {
	if (padChar == LispFormat.PARAM_UNSPECIFIED
	    && padChar == LispFormat.PARAM_UNSPECIFIED
	    && commaChar == LispFormat.PARAM_UNSPECIFIED
	    && commaInterval == LispFormat.PARAM_UNSPECIFIED)
	  {
	    if (printPlus)
	      return gnu.text.RomanIntegerFormat.getInstance(printCommas);
	    else
	      return gnu.text.EnglishIntegerFormat.getInstance(printCommas);
	  }
	base = 10;
      }
    if (minWidth == LispFormat.PARAM_UNSPECIFIED)  minWidth = 1;
    if (padChar == LispFormat.PARAM_UNSPECIFIED)  padChar = ' ';
    if (commaChar == LispFormat.PARAM_UNSPECIFIED)  commaChar = ',';
    if (commaInterval == LispFormat.PARAM_UNSPECIFIED)  commaInterval = 3;
    if (base == 10 && minWidth == 1 && padChar == ' '
	&& commaChar == ',' && commaInterval == 3
	&& ! printCommas && ! printPlus)
      return getInstance();
    LispIntegerFormat fmt = new LispIntegerFormat();
    fmt.base = base;
    fmt.minWidth = minWidth;
    fmt.padChar = padChar;
    fmt.commaChar = commaChar;
    fmt.commaInterval = commaInterval;
    fmt.printCommas = printCommas;
    fmt.printPlus = printPlus;
    return fmt;
  }

  public int format(Object[] args, int start, 
		    Writer dst, FieldPosition fpos) 
    throws java.io.IOException
  {
    int minWidth =  LispFormat.getParam(this.minWidth, 1, args, start);
    if (this.minWidth == LispFormat.PARAM_FROM_LIST)  start++;
    char padChar = LispFormat.getParam(this.padChar, ' ', args, start);
    if (this.padChar == LispFormat.PARAM_FROM_LIST)  start++;
    char commaChar = LispFormat.getParam(this.commaChar, ',', args, start);
    if (this.commaChar == LispFormat.PARAM_FROM_LIST)  start++;
    int commaInterval = LispFormat.getParam(this.commaInterval, 3, args,start);
    if (this.commaInterval == LispFormat.PARAM_FROM_LIST)  start++;
    Object arg = args[start];
    IntNum iarg = LispFormat.asInteger(arg);
    if (iarg != null)
      {
	String sarg = iarg.toString(base);
	boolean neg = sarg.charAt(0) == '-';
	int slen = sarg.length();
	int ndigits = neg ? slen - 1 : slen;
	int numCommas = printCommas ? (ndigits-1)/commaInterval : 0;
	int unpadded_len = ndigits + numCommas;
	if (neg || printPlus)
	  unpadded_len++;
	for (; minWidth > unpadded_len;  --minWidth)
	  dst.write(padChar);
	int i = 0;
	if (neg)
	  {
	    dst.write('-');
	    i++;
	    slen--;
	  }
	else if (printPlus)
	  dst.write('+');
	for (;;)
	  {
	    dst.write(sarg.charAt(i++));
	    if (--slen == 0)
	      break;
	    if (printCommas && (slen % commaInterval) == 0)
	      dst.write(commaChar);
	  }
      }
    else
      print(dst, arg.toString());
    return start + 1;
  }
}

/** Handle formatting of characters.
 * Used to implement the Common List ~C (Character) and ~~ (Tilde)
 * format operators. */

class LispCharacterFormat extends ReportFormat
{
  boolean seenAt;
  boolean seenColon;
  int count;
  int charVal;

  public static LispCharacterFormat
  getInstance(int charVal, int count, boolean seenAt, boolean seenColon)
  {
    LispCharacterFormat fmt = new LispCharacterFormat();
    fmt.count = count;
    fmt.charVal = charVal;
    fmt.seenAt = seenAt;
    fmt.seenColon = seenColon;
    return fmt;
  }

  public int format(Object[] args, int start, 
		    Writer dst, FieldPosition fpos) 
    throws java.io.IOException
  {
    int count = LispFormat.getParam(this.count, 1, args, start);
    if (this.count == LispFormat.PARAM_FROM_LIST)  start++;
    int charVal = LispFormat.getParam(this.charVal, '?', args, start);
    if (this.charVal == LispFormat.PARAM_FROM_LIST)  start++;
    while (--count >= 0)
      printChar (charVal, seenAt, seenColon, dst);
    return start;
  }

  public static void printChar(int ch, boolean seenAt, boolean seenColon,
			       Writer dst)
    throws java.io.IOException
  {
    if (seenAt)
      {
	print(dst, Char.toScmReadableString(ch));
      }
    else if (seenColon)
      {
	if (ch < ' ')
	  {
	    dst.write('^');
	    dst.write(ch + 0x40);
	  }
	else if (ch >= 0x7f)
	  {
	    print(dst, "#\\");
	    print(dst, Integer.toString(ch, 8));
	  }
	else
	  dst.write(ch);
      }
    else
      {
	// if (ch > 0xFFFF) print surrogate chars; else
	dst.write(ch);
      }
  }
}

/** Perform general padding.
 * Used to implement the Common Lisp ~A (Ascii) and ~ (S-expression),
 * format operators, unless they have no parameters. */

class LispObjectFormat extends ReportFormat
{
  int minWidth;
  int colInc;
  int minPad;
  int padChar;
  int where;
  ReportFormat base;

  public LispObjectFormat(ReportFormat base,
			  int minWidth, int colInc, int minPad, int padChar,
			  int where)
  {
    this.base = base;
    this.minWidth = minWidth;
    this.colInc = colInc;
    this.minPad = minPad;
    this.padChar = padChar;
    this.where = where;
  }

  public int format(Object[] args, int start, 
		    Writer dst, FieldPosition fpos) 
    throws java.io.IOException
  {
    int minWidth = LispFormat.getParam(this.minWidth, 0, args, start); 
    if (this.minWidth == LispFormat.PARAM_FROM_LIST)  start++; 
    int colInc = LispFormat.getParam(this.colInc, 1, args, start); 
    if (this.colInc == LispFormat.PARAM_FROM_LIST)  start++; 
    int minPad = LispFormat.getParam(this.minPad, 0, args, start); 
    if (this.minPad == LispFormat.PARAM_FROM_LIST)  start++; 
    char padChar = LispFormat.getParam(this.padChar, ' ', args, start); 
    if (this.padChar == LispFormat.PARAM_FROM_LIST)  start++; 
    return gnu.text.PadFormat.format(base, args, start, dst,
				     padChar, minWidth, colInc, minPad,
				     where, fpos);
  }
}

class LispEscapeFormat extends ReportFormat 
{
  int param1;
  int param2;
  int param3;
  boolean escapeAll;

  public final static LispEscapeFormat alwaysTerminate
  = new LispEscapeFormat(0, LispFormat.PARAM_UNSPECIFIED);

  public LispEscapeFormat(int param1, int param2)
  {
    this.param1 = param1;
    this.param2 = param2;
    this.param3 = LispFormat.PARAM_UNSPECIFIED;
  }

  public LispEscapeFormat(int param1, int param2, int param3)
  {
    this.param1 = param1;
    this.param2 = param2;
    this.param3 = param3;
  }

  static Numeric getParam(int param, Object[] args, int start)
  {
    if (param == LispFormat.PARAM_FROM_COUNT)
      return IntNum.make(args.length - start);
    if (param == LispFormat.PARAM_FROM_LIST)
      {
	Object arg = args[start];
	if (arg instanceof Numeric)
	  return (Numeric) arg;
	if (arg instanceof Number)
	  {
	    if (arg instanceof Float || arg instanceof Double)
	      return new DFloNum(((Number) arg).doubleValue());
	    return IntNum.make(((Number) arg).longValue());
	  }
	if (arg instanceof Char)
	  return new IntNum(((Char) arg).intValue());
	if (arg instanceof Character)
	  return new IntNum((int) ((Character) arg).charValue());
	return new DFloNum(Double.NaN);
      }
    return IntNum.make(param);
  }

  /** WRONG: Tests if we should exit the the surrounding format.
   * Returns 2*ARGS_USED+(DO_TERMINATE?1:0), where ARGS_USED is the
   * number of arguments consumed by the specification, and
   * DO_TERMINATE is true if we actually should exit.
   */
  public int format(Object[] args, int start, Writer dst, FieldPosition fpos)
    throws java.io.IOException
  {
    int orig_start = start;
    boolean do_terminate;
    if (param1 == LispFormat.PARAM_UNSPECIFIED)
      do_terminate = start == args.length;
    else if (param2 == LispFormat.PARAM_UNSPECIFIED && param1 == 0)
      do_terminate = true;  // Efficiency hack
    else
      {
	Numeric arg1 = getParam(param1, args, start);
	if (param1 == LispFormat.PARAM_FROM_LIST)  start++;
	if (param2 == LispFormat.PARAM_UNSPECIFIED)
	  {
	    do_terminate = arg1.isZero();
	  }
	else
	  {
	    Numeric arg2 = getParam(param2, args, start);
	    if (param2 == LispFormat.PARAM_FROM_LIST)  start++;
	    if (param3 == LispFormat.PARAM_UNSPECIFIED)
	      {
		do_terminate = arg1.equals(arg2);
	      }
	    else
	      {
		Numeric arg3 = getParam(param3, args, start);
		if (param3 == LispFormat.PARAM_FROM_LIST)  start++;
		do_terminate = arg2.geq(arg1) && arg3.geq(arg2);
	      }
	  }
      }
    return result(! do_terminate ? 0 : escapeAll ? ESCAPE_ALL : ESCAPE_NORMAL,
		  start);
  }

  public final static int ESCAPE_NORMAL = 0xF1;
  public final static int ESCAPE_ALL = 0xF2;
}

class LispIterationFormat extends ReportFormat
{
  int maxIterations;
  boolean seenAt;
  boolean seenColon;
  boolean atLeastOnce;

  Format body;

  public static int format(Format body, int maxIterations,
			   Object[] args, int start, 
			   Writer dst, boolean seenColon, boolean atLeastOnce)
    throws java.io.IOException
  {
    for (int i = 0; ; i++)
      {
	//System.err.println("it:"+i+" start:"+start+" maxIt:"+maxIterations);
	if (i == maxIterations && maxIterations != -1)
	  break;
	if (start == args.length && (i > 0 || ! atLeastOnce))
	  break;
	//System.err.println("loop:i:"+i+ " start:"+start);
	if (seenColon)
	  {
	    Object curArg = args[start];
	    Object[] curArr = LispFormat.asArray(curArg);
	    if (curArr == null)
	      { // ?
	      }
	    int result = ReportFormat.format(body, curArr, 0, dst, null);
	    start++;
	    if (ReportFormat.resultCode(result) == LispEscapeFormat.ESCAPE_ALL)
	      break;
	  }
	else
	  {
	    start = ReportFormat.format(body, args, start, dst, null);
	    if (start < 0)
	      {
		start = ReportFormat.nextArg(start);
		break;
	      }
	  }
      }
    return start;
  }

  public int format(Object[] args, int start,
                    Writer dst, FieldPosition fpos)  
    throws java.io.IOException
  {
    int maxIterations = LispFormat.getParam(this.maxIterations, -1,
					    args, start);
    if (this.maxIterations == LispFormat.PARAM_FROM_LIST)  start++;

    Format body = this.body;
    if (body == null)
      {
	// from args
	Object arg = args[start++];
	if (arg instanceof java.text.Format)
	  body = (java.text.Format) arg;
	else
	  {
	    try
	      {
		body = new LispFormat(arg.toString());
	      }
	    catch (Exception ex)
	      {
		print(dst, "<invalid argument for \"~{~}\" format>");
		return args.length; // FIXME
	      }
	  }
      }
    if (seenAt)
      {
	return format(body, maxIterations, args, start,
		      dst, seenColon, atLeastOnce);
      }
    else
      {
	Object arg = args[start];
	Object[] curArgs = LispFormat.asArray(arg);
	if (curArgs == null)
	  dst.write("{"+arg+"}".toString());
	else
	  format(body, maxIterations, curArgs, 0, 
		 dst, seenColon, atLeastOnce);
	//System.err.println("after-start:"+(start+1)+" len:"+args.length);
	return start + 1;
      }
    /*
    Object[] curArgs;
    int curStart;
    if (seenAt)
      {
	curArgs = args;
	curStart = start;
      }
    else
      {
	Object arg = args[start];
	curArgs = LispFormat.asArray(arg);
	if (curArgs == null)
	  {
	    dst.write("{"+arg+"}".toString());
	    maxIterations = 0;
	  }
	curStart = 0;
      }      
    */
    /*
    for (int i = 0; ; i++)
      {
	//System.err.println("it:"+i+" curStart:"+curStart);
	if (i == maxIterations && maxIterations != -1)
	  break;
	if (curStart == curArgs.length && (i > 0 || ! atLeastOnce))
	  break;
	//System.err.println("loop:i:"+i+ " curStart:"+curStart);
	if (seenColon)
	  {
	    Object curArg = curArgs[curStart];
	    Object[] curArr = LispFormat.asArray(curArg);
	    if (curArr == null)
	      { // ?
	      }
	    ReportFormat.format(body, curArr, 0, dst, fpos);
	    curStart++;
	  }
	else
	  curStart = ReportFormat.format(body, curArgs, curStart, dst, fpos);
	if (curStart < 0)
	  break;
      }
    return seenAt ? curStart : start+1;
    */
  }
}

class LispChoiceFormat extends ReportFormat
{
  int param;
  boolean lastIsDefault;
  boolean testBoolean;  // choice[0] is selected if arg is false.
  boolean skipIfFalse;
  Format[] choices;

  public int format(Object[] args, int start, Writer dst, FieldPosition fpos)  
    throws java.io.IOException 
  {
    Format fmt;
    if (testBoolean)  // Handles ~:[false~;true~]
      {
	fmt = choices[args[start] == Boolean.FALSE ? 0 : 1];
	start++;
      }
    else if (! skipIfFalse)
      {
	int index = LispFormat.getParam(this.param, LispFormat.PARAM_FROM_LIST,
					args, start);
	if (param == LispFormat.PARAM_FROM_LIST)  start++;
	if (index < 0 || index >= choices.length)
	  {
	    if (lastIsDefault)
	      index = choices.length - 1;
	    else
	      return start;
	  }
	fmt = choices[index];
      }
    else
      {
	if (args[start] == Boolean.FALSE)
	  return start + 1;
	fmt = choices[0];
      }
    return ReportFormat.format(fmt, args, start, dst, fpos);
  }
}

class LispRepositionFormat extends ReportFormat
{
  boolean backwards;
  boolean absolute;
  int count;

  public LispRepositionFormat(int count, boolean backwards, boolean absolute)
  {
    this.count = count;
    this.backwards = backwards;
    this.absolute = absolute;
  }

  public int format(Object[] args, int start, Writer dst, FieldPosition fpos)  
    throws java.io.IOException 
  {
    int count = LispFormat.getParam(this.count, absolute ? 0 : 1,
				    args, start);
    if (!absolute)
      {
	if (backwards)
	  count = -count;
	count += start;
      }
    return count < 0 ? 0 : count > args.length ? args.length : count;
  }
}

class LispTabulateFormat extends ReportFormat
{
  boolean relative;
  int colnum;
  int colinc;

  public LispTabulateFormat(int colnum, int colinc, boolean relative)
  {
    this.colnum = colnum;
    this.colinc = colinc;
    this.relative = relative;
  }

  public int format(Object[] args, int start, Writer dst, FieldPosition fpos)  
    throws java.io.IOException 
  {
    int colnum = LispFormat.getParam(this.colnum, 1, args, start);
    if (this.colnum == LispFormat.PARAM_FROM_LIST)  start++;
    int colinc = LispFormat.getParam(this.colinc, 1, args, start);
    if (this.colinc == LispFormat.PARAM_FROM_LIST)  start++;
    // FIXME - need to figure out dst's current column number!
    if (! relative)
      colnum = 2;
    while (--colnum >= 0)
      dst.write(' ');
    return start;
  }
}

/* Incomplete support for ~F (requires explicit precision). */

class LispRealFormat extends ReportFormat
{
  char op;
  int width;
  int precision;
  int scale;
  int overflowChar;
  int padChar;
  boolean showPlus;

  public int format(Object[] args, int start, Writer dst, FieldPosition fpos)
    throws java.io.IOException
  {
    int width = LispFormat.getParam(this.width, -1, args, start);
    if (this.width == LispFormat.PARAM_FROM_LIST)  start++;
    int precision = LispFormat.getParam(this.precision, -1, args, start);
    if (this.precision == LispFormat.PARAM_FROM_LIST)  start++;
    int scale = LispFormat.getParam(this.scale, 0, args, start);
    if (this.scale == LispFormat.PARAM_FROM_LIST)  start++;
    char padChar = LispFormat.getParam(this.padChar, ' ', args, start); 
    if (this.padChar == LispFormat.PARAM_FROM_LIST)  start++; 
    char overflowChar = LispFormat.getParam(this.overflowChar,
					    '\uFFFF', args, start); 
    if (this.overflowChar == LispFormat.PARAM_FROM_LIST)  start++; 
    double value = ((Number) args[start++]).doubleValue();
    if (precision >= 0)
      {
	java.text.DecimalFormat fmt
	  = new java.text.DecimalFormat();
	// FIXME what if current locale uses ',' for decmal point?
	fmt.setDecimalSeparatorAlwaysShown(true);
	if (scale == 2)
	  fmt.setMultiplier(100);
	else if (scale == 3)
	  fmt.setMultiplier(1000);
	else if (scale != 0)
	  value = value * Math.pow(10.0, (double) scale);
	if (showPlus)
	  fmt.setPositivePrefix("+");
	fmt.setMinimumFractionDigits(precision);
	fmt.setMaximumFractionDigits(precision);
	String str = fmt.format(value);
	int str_len = str.length();
	if (width > 0)
	  {
	    if (str_len < width)
	      {
		for (int i = width - str_len;  --i >= 0; )
		  dst.write(padChar);
	      }
	    else if (str_len > width && overflowChar != '\uFFFF')
	      {
		for (int i = width;  --i >= 0; )
		  dst.write(overflowChar);
		return start;
	      }
	  }
	print(dst, str);
	return start;
      }
    if (scale != 0)
      value = value * Math.pow(10.0, (double) scale);
    print(dst, Double.toString(value));
    return start;
  }
}
