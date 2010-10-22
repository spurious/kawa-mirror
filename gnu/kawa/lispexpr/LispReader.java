package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.math.*;
import gnu.expr.*;
import gnu.kawa.util.GeneralHashTable;

/** A Lexer for reading S-expressions in generic Lisp-like syntax.
 * This class may have outlived its usefulness: It's mostly just a
 * wrapper around a LineBufferedReader plus a helper token-buffer.
 * The functionality should be moved to ReadTable, though it is
 * unclear what to do about the tokenBuffer.
 */

public class LispReader extends Lexer
{
  public LispReader(LineBufferedReader port)
  {
    super(port);
  }

  public LispReader(LineBufferedReader port, SourceMessages messages)
  {
    super(port, messages);
  }

  GeneralHashTable<Integer,Object> sharedStructureTable;

  /** Read a #|...|#-style comment (which may contain other nested comments).
    * Assumes the initial "#|" has already been read.
    */
  final public void readNestedComment (char c1, char c2)
       throws java.io.IOException, SyntaxException
  {
    int commentNesting = 1;
    int startLine = port.getLineNumber();
    int startColumn = port.getColumnNumber();
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
            eofError("unexpected end-of-file in " + c1 + c2
                     + " comment starting here",
                     startLine + 1, startColumn - 1);
	    return;
	  }
      } while (commentNesting > 0);
  }

  /** Get specification of how symbols should be case-folded.
    * @return Either 'P' (means preserve case), 'U' (upcase),
    * 'D' (downcase), or 'I' (invert case).
    */
  static char getReadCase()
  {
    char read_case;
    try
      {
	String read_case_string
	  = Environment.getCurrent().get("symbol-read-case", "P").toString();
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

  public Object readValues (int ch,  ReadTable rtable)
      throws java.io.IOException, SyntaxException
  {
    return readValues(ch, rtable.lookup(ch), rtable);
  }

  /** May return zero or multiple values. */
  public Object readValues (int ch, ReadTableEntry entry, ReadTable rtable)
      throws java.io.IOException, SyntaxException
  {
    // Step numbers refer to steps in section 2.2 of the HyperSpec.
    // Step 1:
    int startPos = tokenBufferLength;

    seenEscapes = false;
    int kind = entry.getKind();
    switch (kind)
      {
      case ReadTable.ILLEGAL:
	// Step 2:
	String err = ("invalid character #\\"+((char) ch));  // FIXME
	if (interactive) fatal(err);
	else error(err);
	return Values.empty;
      case ReadTable.WHITESPACE:
	// Step 3:
	return Values.empty;
      case ReadTable.TERMINATING_MACRO:
      case ReadTable.NON_TERMINATING_MACRO:
	return entry.read(this, ch, -1);
      case ReadTable.CONSTITUENT:
      case ReadTable.SINGLE_ESCAPE: // Step 5:
      case ReadTable.MULTIPLE_ESCAPE: // Step 6:
      default:  // 
	break;
      }
    return readAndHandleToken(ch, startPos, rtable);
  }

  protected Object readAndHandleToken(int ch, int startPos, ReadTable rtable)
    throws java.io.IOException, SyntaxException
  {
    readToken(ch, getReadCase(), rtable);
    int endPos = tokenBufferLength;
    if (! seenEscapes)
      {
        Object value = parseNumber(tokenBuffer, startPos, endPos - startPos,
                                   '\0', 0, SCM_NUMBERS);
        if (value != null && ! (value instanceof String))
          return value;
        /* Common Lisp only?  FIXME
        if (isPotentialNumber(tokenBuffer, startPos, endPos))
          {
            error(value == null ? "not a valid number"
                  : "not a valid number: " + value);
            return IntNum.zero();
          }
        */
      }

    char readCase = getReadCase();
    if (readCase == 'I')
      {
	int upperCount = 0;
	int lowerCount = 0;
	for (int i = startPos;  i < endPos;  i++)
	  {
	    char ci = tokenBuffer[i];
	    if (ci == TOKEN_ESCAPE_CHAR)
	      i++;
	    else if (Character.isLowerCase(ci))
	      lowerCount++;
	    else if (Character.isUpperCase(ci))
	      upperCount++;
	  }
	if (lowerCount == 0)
	  readCase = 'D';
	else if (upperCount == 0)
	  readCase = 'U';
	else
	  readCase = 'P';
      }

    boolean handleUri =
      (endPos >= startPos + 2
       && tokenBuffer[endPos-1] == '}'
       && tokenBuffer[endPos-2] != TOKEN_ESCAPE_CHAR
       && peek() == ':');
    int packageMarker = -1;
    int lbrace = -1, rbrace = -1, braceNesting = 0;
    int j = startPos;
    boolean uriBad = false;
    for (int i = startPos;  i < endPos;  i++)
      {
	char ci = tokenBuffer[i];
	if (ci == TOKEN_ESCAPE_CHAR)
	  {
	    if (++ i < endPos)
	      tokenBuffer[j++] = tokenBuffer[i];
	    continue;
	  }
        if (handleUri)
          {
            if (ci == '{')
              {
                if (lbrace < 0)
                  lbrace = j;
                else if (braceNesting == 0)
                  uriBad = true;
                braceNesting++;
              }
            else if (ci == '}')
              {
                braceNesting--;
                if (braceNesting < 0)
                   uriBad = true;
                else if (braceNesting == 0)
                  {
                    if (rbrace < 0)
                      rbrace = j;
                    else
                      uriBad = true;
                  }
              }
          }
        if (braceNesting > 0)
          ;
	else if (ci == ':')
	  packageMarker = packageMarker >= 0 ? -1 : j;
	else if (readCase == 'U')
	  ci = Character.toUpperCase(ci);
	else if (readCase == 'D')
	  ci = Character.toLowerCase(ci);
	tokenBuffer[j++] = ci;
      }
    endPos = j;

    int len = endPos - startPos;

    if (lbrace >= 0 && rbrace > lbrace)
      {
        String prefix = lbrace > 0 ? new String(tokenBuffer, startPos, lbrace-startPos) : null;
        lbrace++;
        String uri = new String(tokenBuffer, lbrace, rbrace-lbrace);
        ch = read(); // skip ':' - previously peeked.
        ch = read();
        Object rightOperand = readValues(ch, rtable.lookup(ch), rtable);
        if (! (rightOperand instanceof SimpleSymbol))
          error("expected identifier in symbol after '{URI}:'");
        // FIXME should allow "compound keyword" - for attribute names
        return Symbol.valueOf(rightOperand.toString(), uri, prefix);
      }

    if (rtable.initialColonIsKeyword && packageMarker == startPos && len > 1)
      {
	startPos++;
	String str = new String(tokenBuffer, startPos, endPos-startPos);
	return Keyword.make(str.intern());
    }
    if (rtable.finalColonIsKeyword && packageMarker == endPos - 1
        && (len > 1 || seenEscapes))
      {
	String str = new String(tokenBuffer, startPos, len - 1);
	return Keyword.make(str.intern());
      }
    return rtable.makeSymbol(new String(tokenBuffer, startPos, len));
  }

  public static final char TOKEN_ESCAPE_CHAR = '\uffff';

  /** If true, then tokenbuffer contains escaped characters.
   * These are prefixed (in the buffer) by TOKEN_ESCAPE_CHAR.
   */
  protected boolean seenEscapes;

  void readToken(int ch, char readCase, ReadTable rtable)
      throws java.io.IOException, SyntaxException
  {
    boolean inEscapes = false;
    int braceNesting = 0;
    for (;; ch = read())
      {
	if (ch < 0)
	  {
	    if (inEscapes)
	      eofError("unexpected EOF between escapes");
	    else
	      break;
	  }
	ReadTableEntry entry = rtable.lookup(ch);
	int kind = entry.getKind();
	if (kind == ReadTable.ILLEGAL)
	  {
	    if (inEscapes)
	      {
		tokenBufferAppend(TOKEN_ESCAPE_CHAR);
		tokenBufferAppend(ch);
		continue;
	      }
            if (ch == '}' && --braceNesting >= 0)
              {
                tokenBufferAppend(ch);
		continue;
              }
	    unread(ch);
	    break;
	  }
        if (ch == rtable.postfixLookupOperator && ! inEscapes)
          {
            int next = port.peek();
            if (next == rtable.postfixLookupOperator)
              { // Looking at '::'
                unread(ch);
                break;
              }
            if (validPostfixLookupStart(next, rtable))
              kind = ReadTable.TERMINATING_MACRO;
          }
                  
	if (kind == ReadTable.SINGLE_ESCAPE)
	  {
	    ch = read();
	    if (ch < 0)
	      eofError("unexpected EOF after single escape");
            if (rtable.hexEscapeAfterBackslash
                && (ch == 'x' || ch == 'X'))
              ch = readHexEscape();
	    tokenBufferAppend(TOKEN_ESCAPE_CHAR);
	    tokenBufferAppend(ch);
	    seenEscapes = true;
	    continue;
	  }
	if (kind == ReadTable.MULTIPLE_ESCAPE)
	  {
	    inEscapes = ! inEscapes;
	    seenEscapes = true;
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
                if (ch == '{' && entry == ReadTableEntry.brace)
                  braceNesting++;
                /* ... fall thotugh ... */
	      case ReadTable.NON_TERMINATING_MACRO:
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
        ReadTable rtable = ReadTable.getCurrent();
	for (;;)
	  {
	    int line = port.getLineNumber();
	    int column = port.getColumnNumber();
	    int ch = port.read();
	    if (ch < 0)
	      return Sequence.eofValue; // FIXME
            Object value = readValues(ch, rtable);
	    if (value == Values.empty)
	      continue;
	    return handlePostfix(value, rtable, line, column);
	  }
      }
    finally
      {
	tokenBufferLength = startPos;
	((InPort) port).readState = saveReadState;
      }
  }

  protected boolean validPostfixLookupStart (int ch, ReadTable rtable)
      throws java.io.IOException
  {
    if (ch < 0 || ch == ':' || ch == rtable.postfixLookupOperator)
      return false;
    if (ch == ',')
      return true;
    int kind = rtable.lookup(ch).getKind();
    return kind == ReadTable.CONSTITUENT
      || kind == ReadTable.NON_TERMINATING_MACRO
      || kind == ReadTable.MULTIPLE_ESCAPE
      || kind == ReadTable.SINGLE_ESCAPE;
  }

  Object handlePostfix (Object value, ReadTable rtable, int line, int column)
      throws java.io.IOException, SyntaxException
  {
    if (value == QuoteExp.voidExp)
      value = Values.empty;
    for (;;)
      {
        int ch = port.peek();
        if (ch < 0 || ch != rtable.postfixLookupOperator)
          break;
        // A kludge to map PreOpWord to ($lookup$ Pre 'Word).
        port.read();
        int ch2 = port.peek();
        if (! validPostfixLookupStart(ch2, rtable))
          {
            unread();
            break;
          }
        ch = port.read();
        Object rightOperand = readValues(ch, rtable.lookup(ch), rtable);
        value = LList.list2(value,
                            LList.list2(rtable.makeSymbol(LispLanguage.quasiquote_sym), rightOperand));
        value = PairWithPosition.make(LispLanguage.lookup_sym, value,
                                      port.getName(), line+1, column+1);
      }
    return value;
  }

  private boolean isPotentialNumber (char[] buffer, int start, int end)
  {
    int sawDigits = 0;
    for (int i = start;  i < end;  i++)
      {
	char ch = buffer[i];
	if (Character.isDigit(ch))
	  sawDigits++;
	else if (ch == '-' || ch == '+')
	  {
	    if (i + 1 == end)
	      return false;
	  }
	else if (ch == '#')
	  return true;
	else if (Character.isLetter(ch) || ch == '/'
		 || ch == '_' || ch == '^')
 	  {
	    // CommonLisp defines _123 (and ^123) as a "potential number";
	    // most implementations seem to define it as a symbol.
	    // Scheme does defines it as a symbol.
	    if (i == start)
	      return false;
	  }
	else if (ch != '.')
	  return false;
      }
    return sawDigits > 0;
  }

  static final int SCM_COMPLEX = 1;
  public static final int SCM_NUMBERS = SCM_COMPLEX;

  public static Object parseNumber
  /* #ifdef use:java.lang.CharSequence */
  (CharSequence str, int radix)
  /* #else */
  // (CharSeq str, int radix)
  /* #endif */
  {
    char[] buf;
    if (str instanceof FString)
      buf = ((FString) str).data;
    else
      buf = str.toString().toCharArray();
    int len = str.length();
    return parseNumber(buf, 0, len, '\0', radix, LispReader.SCM_NUMBERS);
  }

  /** Parse a number.
   * @param buffer contains the characters of the number
   * @param start startinging index of the number in the buffer
   * @param count number of characters in buffer to use
   * @param exactness either 'i' or 'I' force an inexact result,
   *   either 'e' or 'E' force an exact result,
   *   '\0' yields an inact or inexact depending on the form of the literal,
   *   while ' ' is like '\0' but does not allow more exactness specifiers.
   * @param radix the number base to use or 0 if unspecified
   * @return the number if a valid number; null or a String-valued error
   *   message if if there was some error parsing the number.
   */
  public static Object parseNumber(char[] buffer, int start, int count,
				   char exactness, int radix, int flags)
  {
    int end = start + count;
    int pos = start;
    if (pos >= end)
      return "no digits";
    char ch = buffer[pos++];
    while (ch == '#')
      {
	if (pos >= end)
	  return "no digits";
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
	    if (exactness != '\0')
	      {
		if (exactness == ' ')
		  return "non-prefix exactness specifier";
		else
		  return "duplicate exactness specifier";
	      }
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
		  return "missing letter after '#'";
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
	if (pos >= end)
	  return "no digits";
	ch = buffer[pos++];
      }
    if (exactness == '\0')
      exactness = ' ';
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
    boolean numeratorNegative = negative;
    boolean sign_seen = ch == '-' || ch == '+';
    if (sign_seen)
      {
	if (pos >= end)
	  return "no digits following sign";
	ch = buffer[pos++];
      }

    // Special case for '+i' and '-i'.
    if ((ch == 'i' || ch == 'I') && pos == end && start == pos - 2
	&& (flags & SCM_COMPLEX) != 0)
      {
	char sign = buffer[start];
	if (sign != '+' && sign != '-')
	  return "no digits";
	if (exactness == 'i' || exactness == 'I')
	  return new DComplex(0, negative ? -1 : 1);
	return negative ? Complex.imMinusOne() : Complex.imOne();
      }

    int realStart = pos - 1;
    boolean hash_seen = false;
    int exp_seen = -1;
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
                int exp_pos = pos-1;
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
		if (exp_seen >= 0)
		  return "duplicate exponent";
		if (radix != 10)
		  return "exponent in non-decimal number";
		if (digits_start < 0)
		  return "mantissa with no digits";
		exp_seen = exp_pos;
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
		if (exp_seen >= 0 || decimal_point >= 0)
		  return "fraction symbol '/' following exponent or '.'";
		numerator = valueOf(buffer, digits_start, pos - digits_start,
				    radix, negative, lvalue);
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

    char infnan = '\0';

    if (digits_start < 0)
      {
        if (sign_seen
            && pos + 4 < end && buffer[pos+3] == '.' && buffer[pos+4] == '0')
          {
            if (buffer[pos] == 'i'
                && buffer[pos+1] == 'n'
                && buffer[pos+2] == 'f')
              {
                infnan = 'i';
              }
            else if (buffer[pos] == 'n'
                && buffer[pos+1] == 'a'
                && buffer[pos+2] == 'n')
              {
                infnan = 'n';
              }
          }
        if (infnan == '\0')
          return "no digits";
        pos += 5;
      }

    if (hash_seen || underscore_seen)
      {
	// FIXME make copy, removing '_' and replacing '#' by '0'.
      }

    boolean inexact = (exactness == 'i' || exactness == 'I'
		       || (exactness == ' ' && hash_seen));
    RealNum number = null;
    char exp_char = '\0';
    if (infnan != '\0')
      {
        inexact = true;
	double d = infnan == 'i' ? Double.POSITIVE_INFINITY : Double.NaN;
	number = new DFloNum(negative ? - d : d);
      }
    else if (exp_seen >= 0 || decimal_point >= 0)
      {
	if (digits_start > decimal_point && decimal_point >= 0)
	  digits_start = decimal_point;
	if (numerator != null)
	  return "floating-point number after fraction symbol '/'";
	String str = new String(buffer, digits_start, pos - digits_start);
        if (exp_seen >= 0)
          {
            exp_char = Character.toLowerCase(buffer[exp_seen]);
            if (exp_char != 'e')
              {
                int prefix = exp_seen - digits_start;
                str = str.substring(0, prefix)+'e'+str.substring(prefix+1);
              }
          }
	double d = Convert.parseDouble(str);
	number = new DFloNum(negative ? - d : d);
      }
    else
      {
	IntNum iresult = valueOf(buffer, digits_start, pos - digits_start,
				 radix, negative, lvalue);
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
					  : numeratorNegative ? Double.NEGATIVE_INFINITY
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
	  number = new DFloNum(numeratorNegative && number.isZero() ? -0.0
			       : number.doubleValue());
      }

    if (exactness == 'e' || exactness == 'E')
      number = number.toExact();

    if (pos < end)
      {
	ch = buffer[pos++];

	if (ch == '@')
	  { /* polar notation */
	    Object angle = parseNumber(buffer, pos, end - pos,
				       exactness, 10, flags);
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
	    Object imag = parseNumber(buffer, pos, end - pos,
				      exactness, 10, flags);
	    if (imag instanceof String)
	      return imag;
	    if (! (imag instanceof Complex))
	      return "invalid numeric constant ("+imag+")";
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
        return "excess junk after number";
	
      }
    else if (number instanceof DFloNum && exp_char > 0 && exp_char != 'e')
      {
        double d = number.doubleValue();
        switch (exp_char)
          {
          case 'f':  case 's':
            return Float.valueOf((float) d);
          case 'd':
            return Double.valueOf(d);
          case 'l':
            return java.math.BigDecimal.valueOf(d);
          }
      }
    return number;
  }

  private static IntNum valueOf (char[] buffer, int digits_start,
				 int number_of_digits,
				 int radix, boolean negative,
				 long lvalue)
  {
    // It turns out that if number_of_digits + radix <= 28
    // then the value will fit in a long without overflow,
    // so we can use the value calculated in lvalue.
    if (number_of_digits + radix <= 28)
      return IntNum.make(negative ? - lvalue : lvalue);
    else
      return IntNum.valueOf(buffer, digits_start, number_of_digits,
			    radix, negative);
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
      case ' ':  // Skip to end of line, inclusive.
      case '\n': // Skip initial whitespace on following line.
      case '\r':
      case '\t':
	for (;;)
	  {
	    if (c < 0)
	      {
		eofError("unexpected EOF in literal");
		return -1;
	      }
	    if (c == '\n')
	      break;
	    if (c == '\r')
	      {
		if (peek() == '\n')
		  skip();
                c = '\n';
                break;
	      }
	    if (c != ' ' && c != '\t')
	      {
		unread(c);
		break;
	      }
	    c = read();
	  }
        if (c != '\n')
          break; // ERROR
        // FIXME: if legacy-compatible non-R6RS-mode: return -2;
        for (;;)
          {
            c = read();
            if (c < 0)
              {
                eofError("unexpected EOF in literal");
                return -1;
              }
            if (c != ' ' && c != '\t')
              {
                unread(c);
                return -2;
              }
          }
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
      case 'X':
	return readHexEscape();
      default:  break;
      }
    return c;
  }

  public int readHexEscape ()
    throws java.io.IOException, SyntaxException 
  {
    int c = 0;
    /* A hex escape, as in ANSI C.  */
    for (;;)
      {
        int d = read();
        int v = Character.digit((char) d, 16);
        if (v >= 0)
          c = (c << 4) + v;
        else
          {
            if (d != ';')
              {
                // FIXME: if strict-R6RS: ERROR
                if (d >= 0)
                  unread(d);
              }
            break;
          }
      }
    return c;
  }

  public final Object readObject (int c)
      throws java.io.IOException, SyntaxException
  {
    unread(c);
    return readObject();
  }

  /** Read a "command" - a top-level expression or declaration.
   * Return Sequence.eofValue of end of file. */
  public Object readCommand ()
      throws java.io.IOException, SyntaxException
  {
    return readObject();
  }

  protected Object makeNil ()
  {
    return LList.Empty;
  }

  protected Pair makePair (Object car, int line, int column)
  {
    return makePair(car, LList.Empty, line, column);
  }

  protected Pair makePair (Object car, Object cdr, int line, int column)
  {
    String pname = port.getName();
    if (pname != null && line >= 0)
      return PairWithPosition.make(car, cdr,
                                   pname, line + 1, column + 1);
    else
      return Pair.make(car, cdr);
  }

  protected void setCdr (Object pair, Object cdr)
  {
    ((Pair) pair).setCdrBackdoor(cdr);
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
    reader.readToken(reader.read(), 'P', ReadTable.getCurrent());
    int endPos = reader.tokenBufferLength;
    if (startPos == endPos)
      {
	reader.error("missing numeric token");
	return IntNum.zero();
      }
    Object result = LispReader.parseNumber(reader.tokenBuffer, startPos,
					   endPos - startPos, '\0', radix, 0);
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
    reader.readToken(reader.read(), 'D', ReadTable.getCurrent());
    char[] tokenBuffer = reader.tokenBuffer;
    int length = reader.tokenBufferLength - startPos;
    if (length == 1)
      return Char.make(tokenBuffer[startPos]);
    String name = new String(tokenBuffer, startPos, length);
    ch = Char.nameToChar(name);
    if (ch >= 0)
      return Char.make(ch);
    ch = tokenBuffer[startPos];
    if (ch == 'x' || ch == 'X')
      {
        int value = 0;
        for (int i = 1; ;  i++)
          {
             if (i == length)
	      return Char.make(value);
             int v = Character.digit (tokenBuffer[startPos + i], 16);
             if (v < 0)
               break;
             value = 16 * value + v;
             if (value > 0x10FFFF)
               break;
          }
      }
    ch = Character.digit(ch, 8);
    if (ch >= 0)
      {
	int value = ch;
	for (int i = 1;  ;  i++)
	  {
	    if (i == length)
	      return Char.make(value);
	    ch = Character.digit(tokenBuffer[startPos + i], 8);
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

    /* Handle Unix #!PROGRAM line at start of file. */
    if (ch == '/'
	&& reader.getLineNumber() == 0
	&& reader.getColumnNumber() == 3)
      {
	ReaderIgnoreRestOfLine.getInstance().read(reader, '#', 1);
	return Values.empty;
      }

    int startPos = reader.tokenBufferLength;
    reader.tokenBufferAppend(ch);
    reader.readToken(reader.read(), 'D', ReadTable.getCurrent());
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
      return Special.undefined;
    if (name.equals("abstract"))
      return Special.abstractSpecial;
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
    if (len < 0)
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
