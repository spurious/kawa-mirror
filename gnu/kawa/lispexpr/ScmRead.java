package gnu.kawa.lispexpr;
import gnu.math.*;
import java.io.*;
import gnu.text.SyntaxException;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;
import gnu.kawa.util.*;
import java.util.Vector;

/** A class to read Scheme forms (S-expressions). */

public class ScmRead extends LispReader
{
  public ScmRead(InPort port)
  {
    super(port);
  }
  
  public ScmRead(InPort port, SourceMessages messages)
  {
    super(port, messages);
  }
  
  Object readSymbol ()
       throws java.io.IOException
  {
    return readSymbol (read (), getReadCase());
  }

  protected Object makeNil ()
  {
    return LList.Empty;
  }

  protected Object makePair (Object car, int line, int column)
  {
    PairWithPosition pair = new PairWithPosition (port, car, LList.Empty);
    pair.setLine(line + 1, column + 1);
    pair.setFile (port.getName ());
    return pair;
  }

  protected void setCdr (Object pair, Object cdr)
  {
    ((Pair) pair).cdr = cdr;
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
       throws java.io.IOException
  {
    char firstChar = (char) c;
    if (c < 0 || isDelimiter(firstChar))
      error("no symbol start character");
    StringBuffer str = new StringBuffer (30);
    char lastChar = ' ';
    for (;;)
      {
	char ch = (char)c;
	if (ch == '\\')
	  {
	    c = read ();
	    if (c < 0)
	      {
		error("EOF after \\ escape");
		return "??";
	      }
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
	skip();
      }
    if (lastChar == ':')
      {
	int len = str.length();
	if (len == 2 && str.charAt(0) == ':')
	  return "::";
	str.setLength(len-1);
	return Keyword.make(str.toString().intern());
      }
    if (firstChar == ':')
      {
	str.reverse();
	str.setLength(str.length()-1);
	str.reverse();
	return Keyword.make(str.toString().intern());
      }
    return str.toString().intern();
  }

  /**
   * Read a Scheme character literal.
   * Assumes the initial '#' and '\\' have already been read.
   */
  protected Object readCharacter()
       throws java.io.IOException  
  {
    int c = read ();
    int origc = c;
    if (c < 0)
      {
	error("unexpected EOF in character literal");
	c = '?';
      }
    if (Character.isLowerCase ((char)c) || Character.isUpperCase ((char)c))
      {
	String name = readSymbol(c, 'D').toString();

        c = Char.nameToChar(name);
        if (c < 0)
          {
            if (name.length() > 1)
              {
                error("unknown character name: " + name);
                c = '?';
              }
            else
              c = origc;
          }
      }
    else if ((origc = Character.digit((char) c, 8)) >= 0)
      {
	int next = peek();
	next = Character.digit((char) next, 8);
	if (next >= 0)
	  {
	    c = 8 * origc + next;
	    for (;;)
	      {
		skip();
		next = peek();
		next = Character.digit((char) next, 8);
		if (next < 0)
		  break;
		c = 8 * c + next;
	      }
	  }
      }
    else if (origc == '\r')
      {
	c = '\n';
	if (peek() == '\n')
	  skip();
      }
    return Char.make((char)c);
  }

  UniformVector readUniformVector(char kind)
      throws java.io.IOException, SyntaxException
  {
    int size = readOptionalExponent();
    if (! (size == 8 || size == 16 || size == 32 || size == 64)
        || (kind == 'f' && size < 32)
        || read() != '(')
      {
        error("invalid uniform vector syntax");
        return null;
      }
    Object list = readList(')');
    UniformVector vec = null;
    int len = LList.list_length(list);
    if (len <= 0)
      {
        error("invalid elements in uniform vector syntax");
        return null;
      }
    Sequence q = (Sequence) list;
    switch (kind)
      {
      case 'f':
        switch (size)
          {
          case 32:  return new F32Vector(q);
          case 64:  return new F64Vector(q);
          }
      case 's':
        switch (size)
          {
          case  8:  return new S8Vector(q);
          case 16:  return new S16Vector(q);
          case 32:  return new S32Vector(q);
          case 64:  return new S64Vector(q);
          }
      case 'u':
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

  /** Read a special named constant, assuming "#!" has already been read. */
  protected Object readSpecial()
       throws java.io.IOException
  {
    int c = read ();
    if (c < 0)
      {
	error("unexpected EOF after #!");
	return null;
      }
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
    if (name.equals("default"))
      return Special.dfault;
    if (name.equals("undefined"))
      return Interpreter.undefinedObject;
    if (name.equals("null"))
      return null;
    error("unknown named constant #!"+name);
    return null;
  }

  /** Read a word of alphabetic characters.
   * @param c the first letter of the word (previously read)
   * @return the word that was read.
   */

  public String readAlphaWord (int c)
       throws java.io.IOException
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
	    unread(c);
	    break;
	  }
	c = read();
      }
    return str.toString();
  }

  /** Read a number from this.
   * Actually reads a quantity, which is a complex number and an optional unit.
   * @param radix the default radix
   * This can be overridden by an explicit radix in the number.
   * @return the number read
   */

  public Object readNumber (int radix)
       throws java.io.IOException
  {
    return readNumber (read (), radix);
  }

  /** Resolve a unit name, if possible.
   * Returns null if the unit name is unknown. */
  public Object lookupUnit (String name)
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

  public Object readNumber (int c, int radix)
       throws java.io.IOException
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
	      error("extra exactness specifier (#" + (char)c + ")");
	    exactness = (char) c;
	    break;
	  case 'x':
	  case 'd':
	  case 'o':
	  case 'b':
	    if (explicit_radix != 0)
	      error("extra radix specifier (#" + (char)c + ")");
	    explicit_radix = c == 'x' ? 16 : c == 'd' ? 10 : c == 'o' ? 8 : 2;
	    break;
	  default:
	    error( "unrecognized #-construct in number");
	  }
	c = read ();
      }
    if (explicit_radix != 0)
      radix = explicit_radix;

    Complex cnum = readComplex (c, radix, exactness);
    Object unit = null;
    if (radix == 10)
      {
	c = peek ();
	while (Character.isLowerCase ((char)c)
	       || Character.isUpperCase ((char)c))
	  {
	    skip();
	    String word = readAlphaWord (c);
	    Object u = lookupUnit(word);
	    int power;
	    try {
	      if (peek() == '^')
		{
		  skip();
		  c = peek();
		  if (c != '-' && c != '+'
		      && Character.digit((char)c, 10) < 0)
		    error("missing exponent following unit " + word + '^');
		}
	      power = readOptionalExponent ();
	    } catch (ClassCastException e) {
	      error("unit exponent too large");
	      power = 1;
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
	    c = peek ();
	    if (c == '*')
	      {
		skip();
		c = peek();
	      }
	  }
      }

    if (unit == null)
      return cnum;
    else if (unit instanceof Unit)
      return Quantity.make(cnum, (Unit) unit);
    else
      return LList.list3("*", cnum, unit);
  }

  Complex readComplex (int c, int radix, char exactness)
       throws java.io.IOException
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
    
    RealNum num = readReal (c, radix, exactness);
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
	 * as 0.1).
	 * NOTE:  This requires 2 charactes of look-ahead!
	 */
	if (Character.isLowerCase ((char)(next = peek()))
	    || Character.isUpperCase ((char)next))
	  {
	    unread(next);
	    return num;
	  }
	return Complex.make (IntNum.zero (), num);


      case '@':
	/* polar notation */
	RealNum angle = readReal (read (), radix, exactness);

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
	    im = readReal (c, radix, exactness);
	    if ((c = read ()) != 'i' && c != 'I')
	      error("no i in rectangular complex number");
	  }
	return Complex.make (num, im);
      }

    if (c >= 0)
      unread(c);
    return num;
  }

  /* Signal error(message) and skip to the end of this word. */
  RealNum numError(String message)
    throws java.io.IOException
  {
    error(message);
    for (;;)
      {
	int c = read();
	if (c < 0) break;
	if (isDelimiter((char) c))
	  {
	    unread(c);
	    break;
	  }
      }
    return IntNum.zero();
  }


  public RealNum readReal(int c, int radix, char exactness)
       throws java.io.IOException
  {
    boolean negative = false;

    if (c=='+')
      c = read ();
    else if (c=='-')
      {
	negative = true;
	c = read ();
      }

    int i = port.pos;
    if (c >= 0)
      i--;   // Reset to position before current char c.
    port.pos = i;
    long ival = readDigitsInBuffer(port, radix);
    boolean digit_seen = port.pos > i;
    if (digit_seen && port.pos < port.limit)
      {
	if (isDelimiter(port.buffer[port.pos]))
	  {
	    if (ival >= 0)
	      return IntNum.make(negative ? -ival : ival);
	    else
	      return IntNum.valueOf(port.buffer, i, port.pos - i,
				    radix, negative);
	  }
      }
    StringBuffer str = new StringBuffer (20);
    if (negative)
      str.append('-');
    if (digit_seen)
      str.append(port.buffer, i, port.pos - i);

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
	      return numError("digit after '#' in number");
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
		  return numError("'#' in non-decimal number");
		if (!digit_seen)
		  return numError("'#' with no preceeding digits in number");
		hash_seen = true;
	      }
	    str.append ('0');
	    digit_seen = true;
	    continue;
	  case '.':
	    if (radix != 10)
	      return numError("'.' in non-decimal number");
	    if (point_loc >= 0)
	      return numError("duplicate '.' in number");
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
	      return numError("mantissa with no digits");
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
	  return numError("exponent, '#', or '.' in numerator");
	if (!digit_seen)
	  return numError("numerator with no digits");
	IntNum numer = IntNum.valueOf (str.toString (), radix);
	str.setLength (0);
	c = peek();
	if (Character.digit ((char)c, radix) < 0)
	  return numError("denominator with no digits");
	do
	  {
	    str.append((char) c);
	    skip();
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
		return new DFloNum ((numer.isZero () ? Double.NaN
				     : negative ? Double.NEGATIVE_INFINITY
				     : Double.POSITIVE_INFINITY));
	      }
	    else if (numer.isZero())
	      return numError("0/0 is undefined");
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
      unread(c);

    if (!digit_seen)
      return numError("real number (or component) with no digits");

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
	  return numError("exponent overflow");
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
      throws java.io.IOException, SyntaxException
  {
    StringBuffer obj = new StringBuffer ();
    boolean inString = true;
    int c = '\"';
    int prev;
    do
      {
	int next;

	prev = c;

	// Read next char - inline the common case.
	if (prev == '\r')
	  {
	    c = read();
	    if (c == '\n')
	      continue;
	  }
	else if (port.pos < port.limit && prev != '\n')
	  c = port.buffer[port.pos++];
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
            c = readEscape();
            if (c == -2)
	      {
		c = '\n'; // So prev gets set ...
		continue;
	      }
	    /* ... fall through ... */
	  default:
	    if (c < 0)
	      eofError("unexpected EOF in string literal");
	    obj.append ((char) c);
	    break;
	  }
      } while (inString);
    return new FString (obj);
  }

  protected Object readQuote (String func_symbol)
      throws java.io.IOException, SyntaxException
  {
    return new Pair (func_symbol,
		     new Pair (readObject (), LList.Empty));
  }


  protected FVector readVector ()
    throws java.io.IOException, SyntaxException
  {
    char saveReadState = ((InPort) port).readState;
    ((InPort) port).readState = '(';
     try
       {
	 Vector vec = new Vector();
	 for (;;)
	   {
	     int c = skipWhitespaceAndComments();
	     if (c < 0)
	       eofError("unexpected EOF in vector");
	     if (c == ')')
	       break;
	     vec.addElement(readObject(c));
	   }
	 Object[] objs = new Object[vec.size()];
	 vec.copyInto(objs);
	 return new FVector(objs);
       }
     finally
       {
	((InPort) port).readState = saveReadState;
       }
  }
 

  public static Object readObject(InPort port)
      throws java.io.IOException, SyntaxException
  {
    return (new ScmRead(port)).readObject();
  }

  public Object readObject (int c)
      throws java.io.IOException, SyntaxException
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
		  return LList.Empty;
	      } while (c != '\n' && c!= '\r');
            break;
	  case ')':
	    error("An unexpected close paren was read.");
	  case '(':
	    return readList(')');
	  case '[':
	    return readList(']');
	  case '"':
	    saveReadState = ((InPort) port).readState;
	    ((InPort) port).readState = '\"';
	    try
	      {
		return readString();
	      }
	    finally
	      {
		((InPort) port).readState = saveReadState;
	      }
	  case '\'':
	    return readQuote(Interpreter.quote_sym);
	  case '`':
	    return readQuote(Interpreter.quasiquote_sym);
	  case ',':
	    String func;
	    if (peek()=='@')
	      {
		skip();
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
	      return readNumber(c, 10);
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
                next = peek();
                if (Character.isDigit((char) next))
                  return readUniformVector('f');
		return Interpreter.falseObject;
              case 's':
              case 'u':
                return readUniformVector((char) next);
	      case 'x':
	      case 'd':
	      case 'o':
	      case 'b':
	      case 'i':
	      case 'e':
		unread (next);
		return readNumber ('#', 10);
	      case '|':
		saveReadState = ((InPort) port).readState;
		((InPort) port).readState = '|';
		try
		  {
		    readNestedComment();
		  }
		finally
		  {
		    ((InPort) port).readState = saveReadState;
		  }
		break;
	      default:
		error("An invalid #-construct was read.");
	      }
            break;
	  default:
	    if (Character.isWhitespace((char)c))
	      break;
	    if (Character.isDigit((char)c))
	      return readNumber (c, 10);
	    else
	      return readDefault(c);
	  }
	c = read ();
      }
  }

  // To be overridden by subclasses
  public Object readDefault(int c)
       throws java.io.IOException, SyntaxException
  {
    return readSymbol(c, getReadCase());
  }

}
