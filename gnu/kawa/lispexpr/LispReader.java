package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.math.*;
import gnu.expr.*;

/** A Lexer to reading S-expressions in generic Lisp-like syntax. */

public abstract class LispReader extends Lexer
{
  public LispReader(LineBufferedReader port)
  {
    super(port);
  }

  public LispReader(LineBufferedReader port, SourceMessages messages)
  {
    super(port, messages);
  }

  /** Resolve a unit name, if possible.
   * Returns null if the unit name is unknown. */
  public static Object lookupUnit (String name)
  {
    name = (name + "$unit").intern();
    try
      {
	return Environment.getCurrent().getChecked(name);
      }
    catch (UnboundSymbol ex)
      {
	return name;
      }
  }

  /** Read a #|...|#-style comment (which may contain other nested comments).
    * Assumes the initial "#|" has already been read.
    */
  final public void readNestedComment (char c1, char c2)
       throws java.io.IOException
  {
    int commentNesting = 1;
    do
      {
	int c = read ();
	if (c == '|')
	  {
	    c = read();
	    if (c == c1)
	      commentNesting--;
	  }
	else if (c == c1)
	  {
	    c = read();
	    if (c == c2)
	      commentNesting++;
	  }
	if (c < 0)
	  {
	    error("unexpected eof in " + c1 + c2 + " comment.");
	    return;
	  }
      } while (commentNesting > 0);
  }

  /** Get specification of how symbols should be case-folded.
    * @return Either 'P' (means preserve case), 'U' (upcase),
    * 'D' (downcase, or 'I' (invert case).
    */
  public char getReadCase()
  {
    char read_case;
    try
      {
	String read_case_string
	  = Environment.lookup_global("symbol-read-case").toString();
	read_case = read_case_string.charAt(0);
	if (read_case == 'P') ;
	else if (read_case == 'u')
	  read_case = 'U';
	else if (read_case == 'd' || read_case == 'l' || read_case == 'L')
	  read_case = 'D';
	else if (read_case == 'i')
	  read_case = 'I';
      }
    catch (Exception ex)
      {
	read_case = 'P';
      }
    return read_case;
  }

  protected ReadTable getReadTable () { return ReadTable.getCurrent(); }

  public Object readValues (int ch)
      throws java.io.IOException, SyntaxException
  {
    return readValues(ch, getReadTable().lookup(ch));
  }

  /** May return zero or multiple values. */
  public Object readValues (int ch, ReadTableEntry entry)
      throws java.io.IOException, SyntaxException
  {
    // Step numbers refer to steps in section 2.2 of the HyperSpec.
    // Step 1:
    int startPos = tokenBufferLength;

    if (entry == null)
      {
	// Step 2:
	error("invalid character #\\"+((char) ch));  // FIXME
	return Values.empty;
      }
    int kind = entry.getKind();
    boolean inEscapes = false;
    seenEscapes = false;
    switch (kind)
      {
      case ReadTable.WHITESPACE:
	// Step 3:
	return Values.empty;
      case ReadTable.TERMINATING_MACRO:
      case ReadTable.NON_TERMINATING_MACRO:
	Object value = entry.read(this, ch, -1);
	return value;
      case ReadTable.SINGLE_ESCAPE:
	// Step 5:
	ch = read();
	if (ch < 0)
	  eofError("unexpected EOF after single escape");
	tokenBufferAppend(TOKEN_ESCAPE_CHAR);
	tokenBufferAppend(ch);
	seenEscapes = true;
	ch = read();
	break;
      case ReadTable.MULTIPLE_ESCAPE:
	// Step 6:
	inEscapes = true;
	seenEscapes = true;
	ch = read();
	break;
      default:  // case ReadTable.CONSTITUENT:
	break;
      }

    readToken(ch, inEscapes, getReadCase());
    int endPos = tokenBufferLength;
    if (seenEscapes)
      return returnSymbol(startPos, endPos);
    else
      return handleToken(startPos, endPos);
  }

  public static final char TOKEN_ESCAPE_CHAR = '\uffff';

  /** If true, then tokenbuffer contains escaped characters.
   * These are prefixed (in the buffer) by TOKEN_ESCAPE_CHAR.
   */
  protected boolean seenEscapes;

  /** True if ":IDENTIFIER" should be treated as a keyword. */
  protected boolean initialColonIsKeyword = true;

  /** True if "IDENTIFIER:" should be treated as a keyword. */
  protected boolean finalColonIsKeyword = true;

  public void readToken(int ch, boolean inEscapes, char readCase)
      throws java.io.IOException, SyntaxException
  {
    for (;; ch = read())
      {
	if (ch < 0)
	  {
	    if (inEscapes)
	      eofError("unexpected EOF between escapes");
	    else
	      break;
	  }
	ReadTableEntry entry = getReadTable().lookup(ch);
	if (entry == null)
	  {
	    unread(ch);
	    break;
	  }
	int kind = entry.getKind();
	if (kind == ReadTable.SINGLE_ESCAPE)
	  {
	    ch = read();
	    if (ch < 0)
	      eofError("unexpected EOF after single escape");
	    tokenBufferAppend(TOKEN_ESCAPE_CHAR);
	    tokenBufferAppend(ch);
	    seenEscapes = true;
	    continue;
	  }
	if (kind == ReadTable.MULTIPLE_ESCAPE)
	  {
	    inEscapes = ! inEscapes;
	    continue;
	  }
	if (inEscapes)
	  {
	    // Step 9:
	    tokenBufferAppend(TOKEN_ESCAPE_CHAR);
	    tokenBufferAppend(ch);
	  }
	else
	  {
	    // Step 8:
	    switch (kind)
	      {
	      case ReadTable.CONSTITUENT:
		// ... fall through ...
	      case ReadTable.NON_TERMINATING_MACRO:
		if (readCase == 'U'
		    || (readCase == 'I' && Character.isLowerCase((char) ch)))
		  ch = Character.toUpperCase((char) ch);
		else if (readCase == 'D'
			 || (readCase == 'I'
			     && Character.isUpperCase((char) ch)))
		  ch = Character.toLowerCase ((char) ch);
		tokenBufferAppend(ch);
		continue;
	      case ReadTable.MULTIPLE_ESCAPE:
		inEscapes = true;
		seenEscapes = true;
		continue;
	      case ReadTable.TERMINATING_MACRO:
		unread(ch);
		return;
	      case ReadTable.WHITESPACE:
		// if (readPreservingWhitespace) FIXME
		unread(ch);
		return;
	      }
	  }
      }
  }

  public Object readObject ()
      throws java.io.IOException, SyntaxException
  {
    char saveReadState = ((InPort) port).readState;
    int startPos = tokenBufferLength;
    ((InPort) port).readState = ' ';
    try
      {
	for (;;)
	  {
	    int ch = port.read();
	    if (ch < 0)
	      return Sequence.eofValue; // FIXME
	    Object value = readValues(ch);
	    if (value == Values.empty)
	      continue;
	    if (value == QuoteExp.voidExp)
	      value = Values.empty;
	    return value;
	  }
      }
    finally
      {
	tokenBufferLength = startPos;
	((InPort) port).readState = saveReadState;
      }
  }

  /** If tokenBuffer.substring(startPos,endPos) is a number, return it.
   * If it is a symbol, return null.
   * Otherwise, (i.e. if it is a "potential number") throw an Exception.
   * or return a String - for an error message
   */
  protected Object checkNumber(int startPos, int endPos)
  {
    return parseNumber(tokenBuffer, startPos, endPos - startPos, 0, SCM_NUMBERS);
    /*
    int sawDigits = 0;
    int sawDots = 0;
    for (int i = startPos;  i < endPos;  i++)
      {
	char ch = tokenBuffer[i];
	if (Character.isDigit(ch))
	  sawDigits++;
      }
    if (sawDigits == endPos - startPos)
      gnu.math.IntNum.valueOf (tokenBuffer, startPos, endPos - startPos, 10, false);
    return null;
    */
  }

  static final int SCM_COMPLEX = 1;
  public static final int SCM_NUMBERS = SCM_COMPLEX;

  /** Parse a number.
   * If not a number returns a String (error message) or null.
   * @param radix the number base to use or 0 if unspecified
   */
  public static Object parseNumber(char[] buffer, int start, int count, int radix, int flags)
  {
    char exactness = ' ';
    int end = start + count;
    int pos = start;
    if (pos >= end)
      return null;
    char ch = buffer[pos++];
    for (; ch == '#';  ch = buffer[pos++])
      {
	if (pos >= end)
	  return null;
	ch = buffer[pos++];
	switch (ch)
	  {
	  case 'b':  case 'B':
	    if (radix != 0)
	      return "duplicate radix specifier";
	    radix = 2;
	    break;
	  case 'o':  case 'O':
	    if (radix != 0)
	      return "duplicate radix specifier";
	    radix = 8;
	    break;
	  case 'd':  case 'D':
	    if (radix != 0)
	      return "duplicate radix specifier";
	    radix = 10;
	    break;
	  case 'x':  case 'X':
	    if (radix != 0)
	      return "duplicate radix specifier";
	    radix = 16;
	    break;
	  case 'e':  case 'E':
	  case 'i':  case 'I':
	    if (exactness != ' ')
	      return "duplicate exactness specifier";
	    exactness = ch;
	    break;
	  default:
	    int value = 0;
	    for (;;)
	      {
		int dig = Character.digit(ch, 10);
		if (dig < 0)
		  break;
		value = 10 * value + dig;
		if (pos >= end)
		  return null;
		ch = buffer[pos++];
	      }
	    if (ch == 'R' || ch == 'r')
	      {
		if (radix != 0)
		  return "duplicate radix specifier";
		if (value < 2 || value > 35)
		  return "invalid radix specifier";
		radix = value;
		break;
	      }
	    return "unknown modifier '#" + ch + '\'';
	  }
      }
    if (radix == 0)
      {
	for (int i = count;  ; )
	  {
	    if (--i < 0)
	      {
		// FIXME - should get *read-base* in CommonLisp:
		// radix = *read_base*;
		radix = 10;
		break;
	      }
	    if (buffer[start+i] == '.')
	      {
		radix = 10;
		break;
	      }
	  }
      }

    boolean negative = ch == '-';
    if (ch == '-' || ch == '+')
      {
	if (pos >= end)
	  return null;
	ch = buffer[pos++];
      }

    // Special case for '+i' and '-i'.
    if ((ch == 'i' || ch == 'I') && pos == end && start == pos - 2
	&& (flags & SCM_COMPLEX) != 0)
      {
	char sign = buffer[start];
	if (sign != '+' && sign != '-')
	  return null;
	if (exactness == 'i' || exactness == 'I')
	  return new DComplex(0, negative ? -1 : 1);
	return negative ? Complex.imMinusOne() : Complex.imOne();
      }

    int realStart = pos - 1;
    boolean hash_seen = false;
    char exp_seen = '\000';
    int digits_start = -1;
    int decimal_point = -1;
    boolean copy_needed = false;
    boolean underscore_seen = false;
    IntNum numerator = null;
    long lvalue = 0;
  loop:
    for (;;)
      {
	int digit = Character.digit(ch, radix);
	if (digit >= 0)
	  {
	    if (hash_seen && decimal_point < 0)
	      return "digit after '#' in number";
	    if (digits_start < 0)
	      digits_start = pos - 1;
	    lvalue = radix * lvalue + digit;
	  }
	else
	  {
	    switch (ch)
	      {
		/*
	      case '_':
		underscore_seen = true;
		break;
		*/
		/*
	      case '#':
		if (radix != 10)
		  return "'#' in non-decimal number";
		if (digits_start < 0)
		  return "'#' with no preceeding digits in number";
		hash_seen = true;
		break;
		*/
	      case '.':
		if (decimal_point >= 0)
		  return "duplicate '.' in number";
		if (radix != 10)
		  return "'.' in non-decimal number";
		decimal_point = pos - 1;
		break;
	      case 'e': case 's': case 'f': case 'd': case 'l':
	      case 'E': case 'S': case 'F': case 'D': case 'L':
		if (pos == end || radix != 10)
		  {
		    pos--;
		    break loop;
		  }
		char next = buffer[pos];
		if (next == '+' || next == '-')
		  {
		    if (++ pos >= end
			|| Character.digit(buffer[pos], 10) < 0)
		      return "missing exponent digits";
		  }
		else if (Character.digit(next, 10) < 0)
		  {
		    pos--;
		    break loop;
		  }
		if (exp_seen != '\000')
		  return "duplicate exponent";
		if (radix != 10)
		  return "exponent in non-decimal number";
		if (digits_start < 0)
		  return "mantissa with no digits";
		exp_seen = ch;
		for (;;)
		  {
		    pos++;
		    if (pos >= end || Character.digit(buffer[pos], 10) < 0)
		      break loop;
		  }
	      case '/':
		if (numerator != null)
		  return "multiple fraction symbol '/'";
		if (digits_start < 0)
		  return "no digits before fraction symbol '/'";
		if (exp_seen != '\000' || decimal_point >= 0)
		  return "fraction symbol '/' following exponent or '.'";
		if (pos - digits_start < 18)
		  numerator = IntNum.make(negative ? - lvalue : lvalue);
		else
		  numerator = IntNum.valueOf(buffer, digits_start,
					     pos - digits_start, radix, negative);
		digits_start = -1;
		lvalue = 0;
		negative = false;
		hash_seen = false;
		underscore_seen = false;
		break;
	      default:
		pos--;
		break loop;
	      }
	  }
	if (pos == end)
	  break;
	ch = buffer[pos++];
      }

    if (digits_start < 0)
      return "not a number - no digits";

    if (hash_seen || underscore_seen)
      {
	// FIXME make copy, removing '_' and replacing '#' by '0'.
      }

    boolean inexact = (exactness == 'i' || exactness == 'I'
		       || (exactness == ' ' && hash_seen));
    RealNum number = null;
    if (exp_seen != '\000' || decimal_point >= 0)
      {
	if (digits_start > decimal_point && decimal_point >= 0)
	  digits_start = decimal_point;
	if (numerator != null)
	  return "floating-point number after fraction symbol '/'";
	String str = new String(buffer, digits_start, pos - digits_start);
	double d = Convert.parseDouble(str);
	number = new DFloNum(negative ? - d : d);
      }
    else
      {
	IntNum iresult = pos - digits_start < 18
	  ? IntNum.make(negative ? - lvalue : lvalue)
	  : IntNum.valueOf(buffer, digits_start, pos - digits_start,
			   radix, negative);
	if (numerator == null)
	  number = iresult;
	else
	  {
	    // Check for zero denominator values: 0/0, n/0, and -n/0
	    // (i.e. NaN, Infinity, and -Infinity).
	    if (iresult.isZero ())
	      {
		boolean numeratorZero = numerator.isZero();
		if (inexact)
		  number =  new DFloNum ((numeratorZero ? Double.NaN
					  : negative ? Double.NEGATIVE_INFINITY
					  : Double.POSITIVE_INFINITY));
		else if (numeratorZero)
		  return "0/0 is undefined";
		else
		  number = RatNum.make(numerator, iresult);
	      }
	    else
	      {
		number = RatNum.make(numerator, iresult);
	      }
	  }
	if (inexact && number.isExact())
	  // We want #i-0 or #i-0/1 to be -0.0, not 0.0.
	  number = new DFloNum(negative && number.isZero() ? -0.0
			       : number.doubleValue());
      }

    if (exactness == 'e' || exactness == 'E')
      number = number.toExact();

    if (pos < end)
      {
	ch = buffer[pos++];

	if (ch == '@')
	  { /* polar notation */
	    Object angle = parseNumber(buffer, pos, end - pos, 10, flags);
	    if (angle instanceof String)
	      return angle;
	    if (! (angle instanceof RealNum))
	      return "invalid complex polar constant";
	    RealNum rangle = (RealNum) angle;
	    /* r4rs requires 0@1.0 to be inexact zero, even if (make-polar
	     * 0 1.0) is exact zero, so check for this case.  */
	    if (number.isZero () && !rangle.isExact ())
	      return new DFloNum (0.0);

	    return Complex.polar (number, rangle);
	  }

	if (ch == '-' || ch == '+')
	  {
	    pos--;
	    Object imag = parseNumber(buffer, pos, end - pos, 10, flags);
	    if (imag instanceof String)
	      return imag;
	    if (! (imag instanceof Complex))
	      return "invalid numeric constant";
	    Complex cimag = (Complex) imag;
	    RealNum re = cimag.re();
	    if (! re.isZero())
	      return "invalid numeric constant";
	    return Complex.make(number, cimag.im());
	  }

	int lcount = 0;
	for (;;)
	  {
	    if (! Character.isLetter(ch))
	      {
		pos--;
		break;
	      }
	    lcount++;
	    if (pos == end)
	      break;
	    ch = buffer[pos++];
	  }

	if (lcount == 1)
	  {
	    char prev = buffer[pos-1];
	    if (prev == 'i' || prev == 'I')
	      {
		if (pos < end)
		  return "junk after imaginary suffix 'i'";
		return Complex.make(IntNum.zero (), number);
	      }
	  }
	if (lcount > 0)
	  {
	    Object unit = null;
	    for (;;)
	      {
		String word = new String(buffer, pos - lcount, lcount);
		Object u = lookupUnit(word);

		int power = 1;
		if (pos < end)
		  {
		    ch = buffer[pos];
		    if (ch == '^' && ++pos < end)
		      ch = buffer[pos];
		    boolean neg = ch == '-';
		    if ((ch == '-' || ch == '+') && ++pos < end)
		      ch = buffer[pos];
		    power = 0;
		    for (;;)
		      {
			int d = Character.digit(ch, 10);
			if (d < 0)
			  return "junk after unit name";
			power = 10 * power + d;
			if (++pos == end)
			  break;
			if (power > 1000000)
			  return "unit power too large";
			ch = buffer[pos];
		      }
		    if (neg) power = -power;
		  }

		// "expt" and "*" are too open to name clashes. FIXME.
		if (power != 1)
		  {
		    if (u instanceof Unit)
		      u = Unit.pow((Unit) u, power);
		    else
		      u = LList.list3("expt", u, IntNum.make(power));
		  }
		if (unit == null)
		  unit = u;
		else if (u instanceof Unit && unit instanceof Unit)
		  unit = Unit.mul((Unit) unit, (Unit) u);
		else
		  unit = LList.list3("*", unit, u);
		if (pos >= end)
		  break;
		ch = buffer[pos++];
		if (ch == '*')
		  {
		    if (pos == end)
		      return "end of token after '*'";
		    ch = buffer[pos++];
		  }
		lcount = 0;
		for (;;)
		  {
		    if (! Character.isLetter(ch))
		      {
			pos--;
			break;
		      }
		    lcount++;
		    if (pos == end)
		      break;
		    ch = buffer[pos++];
		  }
		if (lcount == 0)
		  return "excess junk after unit";
	      }

	    if (unit == null)
	      return null; // ??
	    else if (unit instanceof Unit)
	      return Quantity.make(number, (Unit) unit);
	    else
	      return LList.list3("*", number, unit);
	  }
	else
	  return "excess junk after number";
	
      }
    return number;
  }

  protected Object returnSymbol(int startPos, int endPos)
  {
    char readCase = getReadCase();
    if (readCase == 'I')
      {
	int upperCount = 0;
	int lowerCount = 0;
	for (int i = startPos;  i < endPos;  i++)
	  {
	    char ch = tokenBuffer[i];
	    if (ch == TOKEN_ESCAPE_CHAR)
	      i++;
	    else if (Character.isLowerCase(ch))
	      lowerCount++;
	    else if (Character.isUpperCase(ch))
	      upperCount++;
	  }
	if (lowerCount == 0)
	  readCase = 'D';
	else if (upperCount == 0)
	  readCase = 'U';
	else
	  readCase = 'P';
      }

    int packageMarker = -1;
    int j = startPos;
    for (int i = startPos;  i < endPos;  i++)
      {
	char ch = tokenBuffer[i];
	if (ch == TOKEN_ESCAPE_CHAR)
	  {
	    if (++ i < endPos)
	      tokenBuffer[j++] = tokenBuffer[i];
	    continue;
	  }
	if (ch == ':')
	  packageMarker = packageMarker >= 0 ? -1 : j;
	else if (readCase == 'U')
	  ch = Character.toUpperCase(ch);
	else if (readCase == 'D')
	  ch = Character.toLowerCase(ch);
	tokenBuffer[j++] = ch;
      }
    endPos = j;

    int len = endPos - startPos;

    if (initialColonIsKeyword && packageMarker == startPos && len > 1)
      {
	startPos++;
	String str = new String(tokenBuffer, startPos, endPos-startPos);
	return Keyword.make(str.intern());
    }
    if (finalColonIsKeyword && packageMarker == endPos - 1 && len > 1)
      {
	String str = new String(tokenBuffer, startPos, len - 1);
	return Keyword.make(str.intern());
      }
    return makeSymbol(new String(tokenBuffer, startPos, len));
  }

  /*
  public boolean isPotentialNumber(int startPos, int endPos)
  {
    int len = endPos - startPos;
    if (len <= 0)
      return false;
    int prevLetter = 0, curLetter;
    int seenDigit = 0;
    int seenDot = 0;
    if ((string[0] < '0' || string[0] > '9')
        && string[0] != '+' && string[0] != '-'
        && string[0] != '.' && string[0] != '^' && string[0] != '_')
        return 0;
    if (string[len-1] == '+' || string[len-1] == '-') return 0;
  }
  */

  /** Classify and return a token in tokenBuffer from startPos to endPos. */
  public Object handleToken(int startPos, int endPos)
  {
    Object value = checkNumber(startPos, endPos);
    if (value != null && ! (value instanceof String))
      return value;
    return returnSymbol(startPos, endPos);
  }

  /** Reads a C-style String escape sequence.
   * Assume '\\' has already been read.
   * Return the converted character, or -1 on EOF, or -2 to ignore. */
  public int readEscape()
    throws java.io.IOException, SyntaxException 
  {
    int c = read();
    if (c < 0)
      {
	eofError("unexpected EOF in character literal");
	return -1;
      }
    return readEscape(c);
  }

  public final int readEscape(int c)
    throws java.io.IOException, SyntaxException 
  {
    switch ((char) c)
      {
      case 'a':  c =  7;  break;  // alarm/bell
      case 'b':  c =  8;  break;  // backspace
      case 't':  c =  9;  break;  // tab
      case 'n':  c = 10;  break;  // newline
      case 'v':  c = 11;  break;  // vertical tab
      case 'f':  c = 12;  break;  // formfeed
      case 'r':  c = 13;  break;  // carriage return
      case 'e':  c = 27;  break;  // escape
      case '\"': c = 34;  break;  // quote
      case '\\': c = 92;  break;  // backslash
      case ' ': // Skip to end of line, inclusive.
	for (;;)
	  {
	    c = read();
	    if (c < 0)
	      {
		eofError("unexpected EOF in character literal");
		return -1;
	      }
	    if (c == '\n')
	      return -2;
	    if (c == '\r')
	      {
		if (peek() == '\n')
		  skip();
		return -2;
	      }
	    if (c != ' ' && c != '\t')
	      {
		unread(c);
		break;
	      }
	  }
      case '\r':
	if (peek() == '\n')
	  skip();
	return -2;
      case '\n':
	return -2;
      case 'M':
	c = read();
	if (c != '-')
	  {
	    error("Invalid escape character syntax");
	    return '?';
	  }
	c = read();
	if (c == '\\')
	  c = readEscape();
	return c | 0200;
      case 'C':
	c = read();
	if (c != '-')
	  {
	    error("Invalid escape character syntax");
	    return '?';
	  }
	/* ... fall through ... */
      case '^':
	c = read();
	if (c == '\\')
	  c = readEscape();
	if (c == '?')
	  return 0177;
	return c & (0200 | 037);
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
	/* An octal escape, as in ANSI C.  */
	c = c - '0';
	for (int count = 0;  ++count < 3; )
	  {
	    int d = read();
	    int v = Character.digit((char) d, 8);
	    if (v >= 0)
	      c = (c << 3) + v;
	    else
	      {
		if (d >= 0)
		  unread(d);
		break;
	      }
	  }
	break;
      case 'u':
	c = 0;
	for (int i = 4;  --i >= 0; )
	  {
	    int d = read ();
	    if (d < 0)
	      eofError("premature EOF in \\u escape");
	    int v = Character.digit ((char) d, 16);
	    if (v < 0)
	      error("non-hex character following \\u");
	    c = 16 * c + v;
	  }
	break;
      case 'x':
	c = 0;
	/* A hex escape, as in ANSI C.  */
	for (;;)
	  {
	    int d = read();
	    int v = Character.digit((char) d, 16);
	    if (v >= 0)
	      c = (c << 4) + v;
	    else
	      {
		if (d >= 0)
		  unread(d);
		break;
	      }
	  }
	break;
      default:  break;
      }
    return c;
  }

   public final Object readObject (int c)
      throws java.io.IOException, SyntaxException
  {
    unread(c);
    return readObject();
  }

  protected Object makeSymbol (String name)
  {
    return name.intern();
  }

  protected Object makeNil ()
  {
    return LList.Empty;
  }

  protected Object makePair (Object car, int line, int column)
  {
    return PairWithPosition.make(car, LList.Empty,
                                 port.getName(), line + 1, column + 1);
  }

  public Object makePair (Object car, Object cdr)
  {
    Object pair = makePair(car, 0, 0);
    setCdr(pair, cdr);
    return pair;
  }

  protected void setCdr (Object pair, Object cdr)
  {
    ((Pair) pair).cdr = cdr;
  }

  /** Read a number from a LispReader
   * @param previous number of characters already pushed on tokenBuffer
   * @param reader LispReader to read from
   * @param radix base to use or -1 if unspecified
   */
  public static Object readNumberWithRadix(int previous, LispReader reader, int radix)
    throws java.io.IOException, SyntaxException
  {
    int startPos = reader.tokenBufferLength - previous;
    reader.readToken(reader.read(), false, 'P');
    int endPos = reader.tokenBufferLength;
    if (startPos == endPos)
      {
	reader.error("missing numeric token");
	return IntNum.zero();
      }
    Object result = LispReader.parseNumber(reader.tokenBuffer, startPos,
					   endPos - startPos, radix, 0);
    if (result instanceof String)
      {
	reader.error((String) result);
	return IntNum.zero();
      }
    else if (result == null)
      {
	reader.error("invalid numeric constant");
	return IntNum.zero();
      }
    else
      return result;
  }

  public static Object readCharacter (LispReader reader)
    throws java.io.IOException, SyntaxException
  {
    int ch = reader.read();
    if (ch < 0)
      reader.eofError("unexpected EOF in character literal");
    int startPos = reader.tokenBufferLength;
    reader.tokenBufferAppend(ch);
    reader.readToken(reader.read(), false, 'D');
    int length = reader.tokenBufferLength - startPos;
    if (length == 1)
      return Char.make(reader.tokenBuffer[startPos]);
    String name = new String(reader.tokenBuffer, startPos, length);
    ch = Char.nameToChar(name);
    if (ch >= 0)
      return Char.make(ch);
    ch = Character.digit(reader.tokenBuffer[startPos], 8);
    if (ch >= 0)
      {
	int value = ch;
	for (int i = 1;  ;  i++)
	  {
	    if (i == length)
	      return Char.make(value);
	    ch = Character.digit(reader.tokenBuffer[startPos + i], 8);
	    if (ch < 0)
	      break;
	    value = 8 * value + ch;
	  }
      }
    reader.error("unknown character name: " + name);
    return Char.make('?');
  }

  public static Object readSpecial (LispReader reader)
    throws java.io.IOException, SyntaxException
  {
    int ch = reader.read();
    if (ch < 0)
      reader.eofError("unexpected EOF in #! special form");
    int startPos = reader.tokenBufferLength;
    reader.tokenBufferAppend(ch);
    reader.readToken(reader.read(), false, 'D');
    int length = reader.tokenBufferLength - startPos;
    String name = new String(reader.tokenBuffer, startPos, length);
    if (name.equals("optional"))
      return Special.optional;
    if (name.equals("rest"))
      return Special.rest;
    if (name.equals("key"))
      return Special.key;
    if (name.equals("eof"))
      return Special.eof;
    if (name.equals("void"))
      //return Values.empty;
      return QuoteExp.voidExp;
    if (name.equals("default"))
      return Special.dfault;
    if (name.equals("undefined"))
      return Interpreter.undefinedObject;
    if (name.equals("null"))
      return null;
    reader.error("unknown named constant #!"+name);
    return null;
  }

  public static SimpleVector
  readSimpleVector(LispReader reader, char kind)
    throws java.io.IOException, SyntaxException
  {
    int size = 0;
    int ch;
    for (;;)
      {
	ch = reader.read();
	if (ch < 0)
	  reader.eofError("unexpected EOF reading uniform vector");
	int digit = Character.digit((char) ch, 10);
	if (digit < 0)
	  break;
	size = size * 10 + digit;
      }
    if (! (size == 8 || size == 16 || size == 32 || size == 64)
        || (kind == 'F' && size < 32)
        || ch != '(')
      {
        reader.error("invalid uniform vector syntax");
        return null;
      }
    Object list = ReaderParens.readList(reader, '(', -1, ')');
    int len = LList.listLength(list, false);
    if (len <= 0)
      {
        reader.error("invalid elements in uniform vector syntax");
        return null;
      }
    Sequence q = (Sequence) list;
    switch (kind)
      {
      case 'F':
        switch (size)
          {
          case 32:  return new F32Vector(q);
          case 64:  return new F64Vector(q);
          }
      case 'S':
        switch (size)
          {
          case  8:  return new S8Vector(q);
          case 16:  return new S16Vector(q);
          case 32:  return new S32Vector(q);
          case 64:  return new S64Vector(q);
          }
      case 'U':
        switch (size)
          {
          case  8:  return new U8Vector(q);
          case 16:  return new U16Vector(q);
          case 32:  return new U32Vector(q);
          case 64:  return new U64Vector(q);
          }
      }
    return null;
  }
}
