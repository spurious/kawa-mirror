package kawa.lang;
import gnu.math.*;
import java.io.*;

public class InPort extends LineBufferedReader implements Printable
{
  String name;

  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public InPort (Reader in)
  {
    super (in);
  }

  public InPort (Reader in, String name)
  {
    this (in);
    this.name = name;
    this.name = name;
  }

  public InPort (InputStream in)
  {
    super (in);
  }

  public InPort (InputStream in, String name)
  {
    this (in);
    this.name = name;
  }

  public static Reader convertToReader (InputStream in, Object conv)
  {
    if (conv != null && conv != Boolean.TRUE)
      {
	String enc = (conv == Boolean.FALSE ? "8859_1" : conv.toString());
	try
	  {
	    return new java.io.InputStreamReader(in, enc);
	  }
	catch (java.io.UnsupportedEncodingException ex)
	  {
	    throw new RuntimeException("unknown character encoding: "+enc);
	  }
      }
    return new java.io.InputStreamReader(in);
  }

  public InPort (InputStream in, String name, Object conv)
    throws java.io.UnsupportedEncodingException
  {
    this (convertToReader(in, conv), name);
    if (conv == Boolean.FALSE)
      {
	// Use a fixed-size buffer.  This prevents really-long "lines"
	// from causing the buffer to grow to accomodate them.
	try
	  {
	    setBuffer(new char[2048]);
	  }
	catch (java.io.IOException ex) { /* ignored */ }
      }
    else
      setConvertCR(true);
  }

  // For now, this is static.  It should probably be thread-local.
  private static InPort inp = new TtyInPort (System.in, "<stdin>",
					     OutPort.outDefault());

  static public InPort inDefault ()
  {
    Thread thread = Thread.currentThread ();
    if (thread instanceof Future)
      return ((Future) thread).in;
    return inp;
  }

  static public void setInDefault (InPort in)
  {
    Thread thread = Thread.currentThread ();
    if (thread instanceof Future)
      ((Future) thread).in = in;
    else
      inp = in;
  }

  public static InPort openFile(String fname)
    throws java.io.UnsupportedEncodingException,
           java.io.FileNotFoundException
  {
    java.io.InputStream strm = new java.io.FileInputStream(fname);
    strm = new java.io.BufferedInputStream(strm);
    return openFile(strm, fname);
  }

  public static InPort openFile(InputStream strm, String fname)
    throws java.io.UnsupportedEncodingException
  {
    return new InPort(strm, fname,
		      Environment.user().get("port-char-encoding"));
  }

  final boolean isDelimiter (char ch)
  {
    return (Character.isWhitespace (ch)
	    || ch == ')' || ch == '(' || ch == '"' || ch == ';');
  }

  char readState = '\n';
  /** Return a character that indicates what we are currently reading.
    * Returns '\n' if we are not inside read; '\"' if reading a string;
    * '|' if inside a comment; '(' if inside a list; and
    * ' ' if otherwise inside a read. */
  public char getReadState () { return readState; }

  Object readSymbol ()
       throws java.io.IOException, ReadError
  {
    return readSymbol (read (), getReadCase());
  }

  /** Get specification of how symbols should be case-folded.
    * @return Either 'P' (means preserve case), 'U' (upcase),
    * 'D' (downcase, or 'I' (invert case).
    */
  public static char getReadCase()
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

  Object readSymbol (int c, char read_case)
       throws java.io.IOException, ReadError
  {
    if (c < 0 || isDelimiter((char) c))
      throw new ReadError(this, "no symbol start character");
    StringBuffer str = new StringBuffer (30);
    char lastChar = ' ';
    for (;;)
      {
	char ch = (char)c;
	if (ch == '\\')
	  {
	    c = read ();
	    if (c < 0)
	      throw new EofReadError(this, "EOF after \\ escape");
	    ch = (char) c;
	    lastChar = ' ';
	  }
	else
	  {
	    lastChar = ch;
	    if (read_case == 'U'
		|| (read_case == 'I' && Character.isLowerCase(ch)))
	      ch = Character.toUpperCase(ch);
	    else if (read_case == 'D'
		     || (read_case == 'I' && Character.isUpperCase(ch)))
	      ch = Character.toLowerCase (ch);
	  }
	str.append (ch);
	c = peek();
	if (c < 0 || isDelimiter((char) c))
	  break;
	skip_quick();
      }
    if (lastChar == ':')
      {
	str.setLength(str.length()-1);
	return Keyword.make(str.toString());
      }
    return str.toString().intern();
  }

  /**
   * Read a Scheme character literal.
   * Assumes the initial '#' and '\\' have already been read.
   */
  protected Object readCharacter()
       throws java.io.IOException, ReadError  
  {
    int c = read ();
    if (c < 0)
      throw new EofReadError (this, "unexpected EOF in character literal");
    int origc = c;
    if (Character.isLowerCase ((char)c) || Character.isUpperCase ((char)c))
      {
	String name = readSymbol(c, 'D').toString();
        int i = Char.charNames.length; 
        for ( ; ; ) {
           if (--i < 0) {
              break;
           }
           if (Char.charNames[i].equals(name)) {
              c = Char.charNameValues[i];
             break;
           }
        }
        if (i<0) {
           if (name.length()>1) {
	     throw new ReadError (this, "unknown character name: " + name);
           } else {
              c = origc;
           }
        }
      }
    else if (origc == '\r')
      {
	c = '\n';
	if (peek() == '\n')
	  skip_quick();
      }
    return Char.make((char)c);
  }

  /** Read a special named constant, assuming "#!" has already been read. */
  protected Object readSpecial()
       throws java.io.IOException, ReadError  
  {
    int c = read ();
    if (c < 0)
      throw new EofReadError (this, "unexpected EOF after #!");
    String name = readSymbol(c, 'D').toString();
    if (name.equals("optional"))
      return Special.optional;
    if (name.equals("rest"))
      return Special.rest;
    if (name.equals("key"))
      return Special.key;
    if (name.equals("eof"))
      return Special.eof;
    if (name.equals("void"))
      return Values.empty;
    if (name.equals("null"))
      return null;
    throw new ReadError(this, "unknown named constant #!"+name);
  }

  /** Read a word of alphabetic characters.
   * @param c the first letter of the word (previously read)
   * @return the word that was read.
   */

  public String readAlphaWord (int c)
       throws java.io.IOException, ReadError
  {
    StringBuffer str = new StringBuffer(20);
    for (;;)
      {
	if (c < 0)
	  break;
	if (Character.isLowerCase ((char)c) || Character.isUpperCase ((char)c))
	  str.append ((char) c);
	else
	  {
	    unread();
	    break;
	  }
	c = read();
      }
    return str.toString();
  }

  /** Read an optional signed integer.
   * If there is no integer in the input stream, return 1.
   * For excessively large exponents, return Integer.MIN_VALUE
   * or Integer.MAX_VALUE.
   */
  int readOptionalExponent()
       throws java.io.IOException, ReadError
  {
    int sign = read();
    boolean neg = false;
    boolean overflow = false;
    int c;
    if (sign == '+' || sign == '-')
      c = read();
    else
      {
	c = sign;
	sign = 0;
      }
    int value;
    if (c < 0 || (value = Character.digit ((char)c, 10)) < 0)
      {
	if (sign != 0)
	  throw new ReadError (this, "exponent sign not followed by digit");
	value = 1;
      }
    else
      {
	int max = (Integer.MAX_VALUE - 9) / 10;
	for (;;)
	  {
	    c = read();
	    int d = Character.digit ((char)c, 10);
	    if (d < 0)
	      break;
	    if (value > max)
	      overflow = true;
	    value = 10 * value + d;
	  }
      }
    if (c >= 0)
      unread();
    if (sign == '-')
      value = -value;
    if (overflow)
      return sign == '-' ? Integer.MIN_VALUE : Integer.MAX_VALUE;
    return value;
  }

  /** Read a number from this.
   * Actually reads a quantity, which is a complex number and an optional unit.
   * @param radix the default radix
   * This can be overridden by an explicit radix in the number.
   * @return the number read
   */

  public Numeric readSchemeNumber (int radix)
       throws java.io.IOException, ReadError
  {
    return readSchemeNumber (read (), radix);
  }

  public Numeric readSchemeNumber (int c, int radix)
       throws java.io.IOException, ReadError
  {
    char exactness = ' ';
    int explicit_radix = 0;
    while (c == '#')
      {
	c = read ();
	switch (c)
	  {
	  case 'e':
	  case 'i':
	    if (exactness != ' ')
	      throw new ReadError
		(this, "extra exactness specifier (#" + (char)c + ")");
	    exactness = (char) c;
	    break;
	  case 'x':
	  case 'd':
	  case 'o':
	  case 'b':
	    if (explicit_radix != 0)
	      throw new ReadError
		(this, "extra radix specifier (#" + (char)c + ")");
	    explicit_radix = c == 'x' ? 16 : c == 'd' ? 10 : c == 'o' ? 8 : 2;
	    break;
	  default:
	    throw new ReadError (this,  "unrecognized #-construct in number");
	  }
	c = read ();
      }
    if (explicit_radix != 0)
      radix = explicit_radix;

    Complex cnum = readSchemeComplex (c, radix, exactness);
    
    Unit unit = null;
    if (radix == 10)
      {
	c = peek ();
	while (Character.isLowerCase ((char)c)
	       || Character.isUpperCase ((char)c))
	  {
	    skip_quick();
	    String word = readAlphaWord (c);
	    Unit u = Unit.lookup (word);
	    if (u == null)
	      throw new ReadError (this, "unknown unit: " + word);
	    int power;
	    try {
	      power = readOptionalExponent ();
	    } catch (ClassCastException e) {
	      throw new ReadError (this, "unit exponent too large");
	    }
	    if (power != 1)
	      u = Unit.pow (u, power);
	    if (unit == null)
	      unit = u;
	    else
	      unit = Unit.mul (unit, u);
	    c = peek ();
	  }
      }

    return unit == null ? cnum : Quantity.make (cnum, unit);
  }

  Complex readSchemeComplex (int c, int radix, char exactness)
       throws java.io.IOException, ReadError
  {
    int next;
    if (((c == '+') || (c == '-'))
	&& (((next = peek ()) == 'i') || (next == 'I')))
      {
	skip();
	if (exactness == 'i')
	  return new DComplex (0, (c == '+') ? 1 : -1);
	else
	  return (c == '+') ? Complex.imOne () : Complex.imMinusOne ();
      }
    
    RealNum num = readSchemeReal (c, radix, exactness);
    c = read ();
    switch (c)
      {
      case 'i': case 'I':
	/* A pure imaginary number.
	 * But if a unit follows, assume the i is part of its name.
	 * This makes it possible to use 12in as 12 inches, not 12i
	 * n's.  To get 12i n's, use 0+12in.
	 * The output methods do not account for this, so read-write
	 * invariance is broken.
	 * We really need a better syntax for quantities, like number*unit.
	 * (Another problem is 1 hertz, which prints as 1s-1, but that reads 
	 * as 0.1)
	 */
	if (Character.isLowerCase ((char)(next = peek()))
	    || Character.isUpperCase ((char)next))
	  {
	    unread();
	    return num;
	  }
	return Complex.make (IntNum.zero (), num);


      case '@':
	/* polar notation */
	RealNum angle = readSchemeReal (read (), radix, exactness);

	/* r4rs requires 0@1.0 to be inexact zero, even if (make-polar
	 * 0 1.0) is exact zero, so check for this case.  */
	if (num.isZero () && !angle.isExact ())
	  return new DFloNum (0.0);

	return Complex.polar (num, angle);
	
      case '+': case '-': 
	/* rectangular notation */
	RealNum im;
	if (((next = peek ()) == 'i') || (next == 'I'))
	  {
	    im = (c == '+') ? IntNum.one () : IntNum.minusOne ();
	    read ();
	  }
	else
	  {
	    im = readSchemeReal (c, radix, exactness);
	    if ((c = read ()) != 'i' && c != 'I')
	      throw new ReadError (this, "no i in rectangular complex number");
	  }
	return Complex.make (num, im);
      }

    if (c >= 0)
      unread_quick();
    return num;
  }

  /** Read digits, up to the first non-digit or the buffer limit
    * @return the digits seen as a non-negative long, or -1 on overflow
    */
  public long readDigits (int radix)
  {
    long ival = 0;
    boolean overflow = false;
    long max_val = Long.MAX_VALUE / radix;
    int i = pos;
    if (i >= limit)
      return 0;
    for (;;)
      {
	char c = buffer[i];
	int dval = Character.digit(c, radix);
	if (dval < 0)
	  break;
	if (ival > max_val)
	  overflow = true;
	else
	  ival = ival * radix + dval;
	if (ival < 0)
	  overflow = true;
	if (++i >= limit)
	  break;
      }
    this.pos = i;
    return overflow ? -1 : ival;
  }

  RealNum readSchemeReal (int c, int radix, char exactness)
       throws java.io.IOException, ReadError
  {
    boolean negative = false;

    if (c=='+')
      c = read ();
    else if (c=='-')
      {
	negative = true;
	c = read ();
      }

    int i = this.pos;
    if (c >= 0)
      i--;   // Reset to position before current char c.
    this.pos = i;
    long ival = readDigits(radix);
    boolean digit_seen = this.pos > i;
    if (digit_seen && this.pos < this.limit)
      {
	if (isDelimiter(this.buffer[this.pos]))
	  {
	    if (ival >= 0)
	      return IntNum.make(negative ? -ival : ival);
	    else
	      return IntNum.valueOf(this.buffer, i, this.pos - i,
				    radix, negative);
	  }
      }
    StringBuffer str = new StringBuffer (20);
    if (negative)
      str.append('-');
    if (digit_seen)
      str.append(this.buffer, i, this.pos - i);

   /* location of decimal point in str.  */
    int point_loc = -1;
    int exp = 0;
    boolean hash_seen = false;
    boolean exp_seen = false;
    for (;;)
      {
	c = read ();
	if (Character.digit ((char)c, radix) >= 0)
	  {
	    if (hash_seen)
	      throw new ReadError (this, "digit after '#' in number");
	    digit_seen = true;
	    str.append ((char) c);
	    continue;
	  }
	switch (c)
	  {
	  case '#':
	    if (!hash_seen)
	      {
		if (radix != 10)
		  throw new ReadError (this, "'#' in non-decimal number");
		if (!digit_seen)
		  throw new ReadError
		    (this, "'#' with no preceeding digits in number");
		hash_seen = true;
	      }
	    str.append ('0');
	    digit_seen = true;
	    continue;
	  case '.':
	    if (radix != 10)
	      throw new ReadError (this, "'.' in non-decimal number");
	    if (point_loc >= 0)
	      throw new ReadError (this, "duplicate '.' in number");
	    point_loc = str.length ();
	    str.append ('.');
	    continue;
	  case 'e': case 's': case 'f': case 'd': case 'l':
	  case 'E': case 'S': case 'F': case 'D': case 'L':
	    int next;
	    if (radix != 10 || !((next = peek ()) == '+' || next == '-'
				 || Character.digit ((char)next, 10) >= 0))
	      break;
	    if (!digit_seen)
	      throw new ReadError (this, "mantissa with no digits");
	    exp = readOptionalExponent ();
	    exp_seen = true;
	    c = read ();
	    break;
	  }
	break;
      }

    if (c == '/')
      {
	if (hash_seen || exp_seen || point_loc != -1)
	  throw new ReadError (this, "exponent, '#', or '.' in numerator");
	if (!digit_seen)
	  throw new ReadError (this, "numerator with no digits");
	IntNum numer = IntNum.valueOf (str.toString (), radix);
	str.setLength (0);
	c = peek();
	if (Character.digit ((char)c, radix) < 0)
	  throw new ReadError (this, "denominator with no digits");
	do
	  {
	    str.append((char) c);
	    skip_quick();
	    c = peek();
	  }
	while (Character.digit ((char)c, radix) >= 0);
	
	IntNum denom = IntNum.valueOf (str.toString (), radix);

	// Check for zero denominator values: 0/0, n/0, and -n/0
	// (i.e. NaN, Infinity, and -Infinity).
	if (denom.isZero ())
	  {
	    if (exactness == 'i')
	      {
		double zero = 0.0; // work-around for a javac bug.
		return new DFloNum ((numer.isZero () ? 0.0
				     : negative ? -1.0 : 1.0)
				    / zero);
	      }
	    else if (numer.isZero())
	      throw new ReadError (this, "0/0 is undefined");
	  }

	if (exactness != 'i')
	  return (RatNum.make (numer, denom));

	// For inexact, we make a RatNum and let it do the division.
	// This doesn't work for #i-0/1, because it's the same
	// rational number as #i0/1, so we check for that.
	return new DFloNum (numer.isZero () ? (negative ? -0.0 : 0.0)
			    : RatNum.make (numer, denom).doubleValue ());

      }
    
    if (c >= 0)
      unread ();

    if (!digit_seen)
      throw new ReadError (this, "real number (or component) with no digits");

    if (exactness == 'i'
	|| exactness == ' ' && (hash_seen || exp_seen || point_loc != -1))
      {
	if (radix == 10)
	  {
	    if (exp != 0)
	      {
		str.append('e');
		str.append(exp);
	      }
	    return new DFloNum(str.toString ());
	  }
	IntNum inum = IntNum.valueOf (str.toString (), radix);
	return new DFloNum (inum.isZero () ? (negative ? -0.0 : 0.0)
			    : inum.doubleValue ());
      }

    if (point_loc == -1 && ! exp_seen)
      return IntNum.valueOf (str.toString (), radix);

    /* Parse an exact with a decimal point or exponent.  */
    IntNum mant;
    if (point_loc == -1)
      mant = IntNum.valueOf (str.toString ());
    else
      {
	String s = str.toString ();
	mant = IntNum.valueOf (s.substring (0, point_loc)
                               + s.substring (point_loc + 1, s.length ()));
	point_loc = s.length () - (point_loc + 1);  // # of decimals
	if (exp < 0 && Integer.MIN_VALUE + point_loc >= exp)
	  throw new ReadError(this, "exponent overflow");
	exp -= point_loc;
      }
    return (mant.isZero () ? mant
	    : (RealNum)(mant.mul (IntNum.power(IntNum.make (10), exp))));
  }

  /**
   * Read a Scheme string literal.
   * Assume we have already skipped the initial '"'.
   */
  protected Object readString()
      throws ReadError,
             java.io.IOException
  {
    StringBuffer obj = new StringBuffer ();
    boolean inString = true;
    int c = '\"';
    int prev;
    do
      {
	int next, v;

	prev = c;

	// Read next char - inline the common case.
	if (prev == '\r')
	  {
	    c = read();
	    if (c == '\n')
	      continue;
	  }
	else if (this.pos < this.limit && prev != '\n')
	  c = this.buffer[this.pos++];
	else
	  c = read();

	switch (c)
	  {
	  case '"':
	    inString = false;
            break;
	  case '\r':
	    obj.append('\n');
	    continue;
	  case '\\':
	    switch (c = read())
	      {
	      case 'a':  v = '\007';  break;
	      case 'b':  v = '\b';    break;
	      case 'f':  v = '\f';    break;
	      case 'n':  v = '\n';    break;
	      case 'r':  v = '\r';    break;
	      case 't':  v = '\t';    break;
	      case 'v':  v = '\013';  break;

	      case 'u':
		v = 0;
		for (int i = 4;  --i >= 0; )
		  {
		    c = read ();
		    if (c < 0)
		      throw new EofReadError (this,
					      "premature EOF in \\u escape");
		    int d = Character.digit ((char) c, 16);
		    if (d < 0)
		      throw new ReadError (this,
					   "non-hex character following \\u");
		    v = 16 * v + d;
		  }
		break;
	      case '0':  case '1':  case '2':  case '3':
	      case '4':  case '5':  case '6':  case '7':
		v = c - '0';
		if ((next = read()) >= 0)
		  {
		    next -= '0';
		    if ((char) next > '\007')
		      unread_quick();
		    else
		      {
			v = v * 8 + next;
			if ((next = read()) >= 0)
			  {
			    next -= '0';
			    if ((char) next > '\007')
			      unread_quick();
			    else
			      v = v * 8 + next;
			  }
		      }
		  }
		break;
	      case '\r':
		if (peek() == '\n')
		  skip_quick();
		continue;
	      case '\n':
		continue;
	      case '"':
	      case '\\':
		v = c;
		break;
	      default:
		if (c < 0)
		  throw new EofReadError (this,
					  "unexpected EOF in string literal");
		throw new ReadError (this, "bad string escape");
	      }
	    obj.append ((char) v);
            break;
	  default:
	    if (c < 0)
	      throw new EofReadError (this,
				      "unexpected EOF in string literal");
	    obj.append ((char) c);
	    break;
	  }
      } while (inString);
    return new FString (obj);
  }

  protected Object readQuote (String func_symbol)
      throws java.io.IOException, ReadError
  {
    return new Pair (func_symbol,
		     new Pair (readSchemeObject (), List.Empty));
  }

  /** Read a #|...|#-style comment (which may contain other nested comments).
    * Assumes the initial "#|" has already been read.
    */
  final void readNestedComment ()
       throws java.io.IOException, ReadError
  {
    int commentNesting = 1;
    do
      {
	int c = read ();
	if (c == '|')
	  {
	    c = read();
	    if (c == '#')
	      commentNesting--;
	  }
	else if (c == '#')
	  {
	    c = read();
	    if (c == '|')
	      commentNesting++;
	  }
	if (c < 0)
	  throw new EofReadError (this, "unexpected eof in #| comment.");
      } while (commentNesting > 0);
  }


  protected void skipWhitespaceAndComments()
      throws java.io.IOException, ReadError
  {
    int c;
    for (;;)
      {
	c = read ();
	if (c < 0)
	  return;
	if (c == ';')
	  {
	    for (;;)
	      {
		c = read ();
		if (c < 0)
		  return;
		if (c == '\n' || c == '\r')
		  break;
	      }
	  }
	else if (c == '#' && peek() == '|')
	  {
	    read();
	    readNestedComment();
	    continue;
	  }
	else if (!Character.isWhitespace ((char)c))
	  break;
      }
    unread_quick ();
  }

 /** Read a list (possibly improper) of zero or more Scheme forms.
   * Assumes '(' has been read.  Does not read the final ')'.
   */
  protected Object readListBody ()
       throws java.io.IOException, ReadError
  {
    Pair last = null;
    List list = List.Empty;

    for (;;)
      {
	skipWhitespaceAndComments();
	int c = peek ();
	if (c == ')' || c < 0)
	  break;
	int line = getLineNumber ();
	int column = getColumnNumber ();

	skip_quick ();
	if (c == '.')
	  {
	    int next = peek ();
	    if (next < 0)
	      throw new EofReadError (this, ". followed by EOF");
	    if (isDelimiter((char)next))
	      {
		// if (last == null && pedantic)
		//   throw new ReadError (this, ". at start of list");
		//-- Read the cdr for the Pair
		Object cdr = readSchemeObject ();
		skipWhitespaceAndComments();
		if (peek () != ')')
		  throw new ReadError (this, ". OBJECT not followed by )");

		// ( a1 ... an . cdr) creates an n-element list ended by
		// cdr.  If n==0, a reasonable (and common) extension is to
		// interpret this as a 0-element list ended by cdr - i.e.
		// just cdr by itself.
		if (last == null)
		  return cdr;

		last.cdr = cdr;
		return list;
	      }
	  }

	Object car = readSchemeObject (c);
	PairWithPosition pair = new PairWithPosition (this, car, List.Empty);
	pair.setLine(line + 1, column + 1);
	pair.setFile (getName ());
	if (last == null)
	  list = pair;
	else
	  last.cdr = pair;
	last = pair;
      }
    return list;
  }

  protected Object readList ()
       throws java.io.IOException, ReadError
  {
    char saveReadState = readState;
    readState = '(';
    try
      {
	Object list = readListBody ();
	int c = read ();
	if (c < 0)
	  throw new EofReadError (this, "unexpected EOF in list");
	return list;
      }
    finally
      {
	readState = saveReadState;
      }
  }

  protected Vector readVector ()
    throws java.io.IOException, ReadError
  {
    char saveReadState = readState;
    readState = '(';
     try
       {
	 java.util.Vector vec = new java.util.Vector();
	 for (;;)
	   {
	     skipWhitespaceAndComments();
	     int c = read();
	     if (c < 0)
	       throw new EofReadError(this, "unexpected EOF in vector");
	     if (c == ')')
	       break;
	     vec.addElement(readSchemeObject(c));
	   }
	 Object[] objs = new Object[vec.size()];
	 vec.copyInto(objs);
	 return new Vector(objs);
       }
     finally
       {
	readState = saveReadState;
       }
  }
 

  public Object readSchemeObject ()
      throws java.io.IOException, ReadError
  {
    char saveReadState = readState;
    readState = ' ';
    try
      {
	return readSchemeObject (read ());
      }
    finally
      {
	readState = saveReadState;
      }
  }
  public Object readSchemeObject (int c)
      throws java.io.IOException, ReadError
  {
    char saveReadState;
    for (;;)
      {
	int next;
	switch (c)
	  {
	  case -1:
	    return Sequence.eofValue;
	  case ';':
	    do
	      {
		c = read();
		if (c < 0) // EOF
		  return List.Empty;
	      } while (c != '\n' && c!= '\r');
            break;
	  case ')':
	    throw new ReadError (this, "An unexpected close paren was read.");
	  case '(':
	    return readList();
	  case '"':
	    saveReadState = readState;
	    readState = '\"';
	    try
	      {
		return readString();
	      }
	    finally
	      {
		readState = saveReadState;
	      }
	  case '\'':
	    return readQuote(Interpreter.quote_sym);
	  case '`':
	    return readQuote(Interpreter.quasiquote_sym);
	  case ',':
	    String func;
	    if (peek()=='@')
	      {
		skip_quick();
		func = Interpreter.unquotesplicing_sym;
	      }
	    else
	      func = Interpreter.unquote_sym;
	    return readQuote (func);
	  case '.':
	  case '+':
	  case '-':
	    next = peek ();
	    if (Character.isDigit((char) next)
		 || (c != '.' && (next == '.' || next == 'i' || next == 'I')))
	      return readSchemeNumber(c, 10);
	    else
	      return readSymbol(c, getReadCase());
	  case '#':
	    next = read();
	    switch (next)
	      {
	      case '(':
		return readVector();
	      case '\\':
		return readCharacter();
	      case '!':
		return readSpecial();
	      case 't':
		return Interpreter.trueObject;
	      case 'f':
		return Interpreter.falseObject;
	      case 'x':
	      case 'd':
	      case 'o':
	      case 'b':
	      case 'i':
	      case 'e':
		unread ();
		return readSchemeNumber ('#', 10);
	      case '|':
		saveReadState = readState;
		readState = '|';
		try
		  {
		    readNestedComment();
		  }
		finally
		  {
		    readState = saveReadState;
		  }
		break;
	      default:
		throw new ReadError (this, "An invalid #-construct was read.");
	      }
            break;
	  default:
	    if (Character.isWhitespace((char)c))
	      break;
	    else if (Character.isDigit((char)c))
	      return readSchemeNumber (c, 10);
	    else
	      return readSymbol(c, getReadCase());
	  }
	c = read ();
      }
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<input-port");
    if (name != null)
      {
	ps.print (' ');
	ps.print (name);
      }
    ps.print ('>');
  }
}
