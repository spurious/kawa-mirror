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

  public InPort (InputStream in)
  {
    super (in);
  }

  public InPort (Reader in)
  {
    super (in);
  }

  public InPort (InputStream in, String name)
  {
    this (in);
    this.name = name;
  }

  public InPort (Reader in, String name)
  {
    this (in);
    this.name = name;
  }

  // For now, this is static.  It should probably be thread-local.
  private static InPort inp = new TtyInPort (System.in, "<stdin>",
					     OutPort.outDefault());

  static public InPort inDefault ()
  {
    return inp;
  }

  static public void setInDefault (InPort in)
  {
    inp = in;
  }

  /* Compatibility. */
  public int readChar ()
       throws java.io.IOException
  {
    return read ();
  }

  Object readSymbol ()
       throws java.io.IOException, ReadError
  {
    return readSymbol (read ());
  }

  Object readSymbol (int c)
       throws java.io.IOException, ReadError
  {
    StringBuffer str = new StringBuffer (30);
    char lastChar = ' ';
    for (;;)
      {
	if (c < 0)
	  break;
	char ch = (char)c;
	if (Character.isWhitespace (ch)
	    || ch == ')' || ch == '(' || ch == '"' || ch == ';')
	  {
	    unread ();
	    break;
	  }
	if (ch == '\\')
	  {
	    c = read ();
	    if (c < 0)
	      break;  // Error
	    ch = (char) c;
	    lastChar = ' ';
	  }
	else

	  {
	    lastChar = ch;
	    ch = Character.toLowerCase (ch);
	  }
	str.append (ch);
	c = read ();
      }
    if (lastChar == ':')
      {
	str.setLength(str.length()-1);
	return Keyword.make(str.toString());
      }
    return Symbol.make (str.toString ());
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
	String name = readSymbol(c).toString();
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
    return Char.make((char)c);
  }

  /** Read a special named constant, assuming "#!" has already been read. */
  protected Object readSpecial()
       throws java.io.IOException, ReadError  
  {
    int c = read ();
    if (c < 0)
      throw new EofReadError (this, "unexpected EOF after #!");
    String name = readSymbol(c).toString();
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

  /** Read an optional signed int.
   * If there is no int in the input stream, return 1.
   */

  int readOptionalExponent()
       throws java.io.IOException, ReadError
  {
    int sign = read();
    boolean neg = false;
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
	for (;;)
	  {
	    c = read();
	    int d = Character.digit ((char)c, 10);
	    if (d < 0)
	      break;
	    value = 10 * value + d;
	  }
      }
    if (c >= 0)
      unread();
    if (sign == '-')
      value = -value;
    return value;
  }

  /** Read a number (with a <prefix> in Scheme syntax from this.
   * @param radix the radix/base specified, or zero if it specified.
   * @param exactness 'i' if #i was seen;  'e' if #i was seen;  else ' '
   *  (currently ignored)
   * @return the number read
   */

  public Numeric readSchemeNumber(int radix, char exactness)
       throws java.io.IOException, ReadError
  {
    return readSchemeNumber (read(), radix, exactness);
  }

  public Numeric readSchemeNumber(int c, int radix, char exactness)
       throws java.io.IOException, ReadError
  {
    while (c == '#')
      {
	c = read();
	switch (c)
	  {
	  case 'e':
	  case 'i':
	    if (exactness != ' ')
	      throw new ReadError (this,  "extra exactness specifier (#"+
				   (char)c+")");
	    exactness = (char) c;
	    break;
	  case 'x':
	  case 'o':
	  case 'b':
	    if (radix != 0)
	      throw new ReadError (this,  "extra radix specifier (#"+
				   (char)c+")");
	    radix = c == 'x' ? 16 : c == 'o' ? 8 : 2;
	    break;
	  default:
	    throw new ReadError (this,  "unrecognized #-construct in number");
	  }
	c = read ();
      }
    if (radix == 0)
      radix = 10;
    Quantity num = readSchemeReal (c, radix, exactness);
    c = read();
    if (c == '@')
      {
	Quantity im = readSchemeReal (read(), radix, exactness);
	return Quantity.add (num, Quantity.mul (im, Complex.imOne()), 1);
      }
    else if (c == '+')
      {
	Quantity im = readSchemeReal (read(), radix, exactness);
	return Quantity.add (num, im, 1);
      }
    else if (c == '-')
      {
	Quantity im = readSchemeReal (read(), radix, exactness);
	return Quantity.add (num, im, -1);
      }
    else if (Character.isLowerCase ((char)c)
	     || Character.isUpperCase ((char)c))
      {
	throw new ReadError (this,
			     "unexpected latter '"+((char)c)+"'after number");
      }
    else if (c >= 0)
      unread();
    return num;
  }

  /* This actually also handles a real followed by 'i' - for now. */
  public Quantity readSchemeReal(int c, int radix, char exactness)
       throws java.io.IOException, ReadError
  {
    int isFloat = 0;  // 1 if seen '.'; 2 if seen exponent
    StringBuffer str = new StringBuffer(20);
    if (c=='+')
      {
	c = read();
      }
    else if (c=='-')
      {
	str.append ((char) c);
	c = read ();
      }

    int digits = 0;
    Unit unit = null;
    boolean imaginary = false;
    for (;;)
      {
	int next;
	if (Character.digit ((char)c, radix) >= 0)
	  digits++;
	else if (c == '.')
	  {
	    if (isFloat > 0)
	      throw new ReadError (this, "unexpected '.' in number");
	    isFloat = 1;
	  }
	else if (radix == 10 && isFloat < 2
		 && (c == 'e' || c == 's' || c == 'f' || c == 'd' || c == 'l'||
		     c == 'E' || c == 'S' || c == 'F' || c == 'D' || c == 'L')
		 && ((next = peek()) == '+' || next == '-'
		     || Character.digit((char)next, 10) >= 0))
	  {
	    isFloat = 2;
	    str.append('e');
	    str.append(readOptionalExponent());
	    c = read();
	    break;
	  }
	else // catches EOF
	  break;
	str.append((char) c);
	c = read();
      }
    if (digits == 0)
      throw new ReadError (this, "number constant with no digits");
    if (c == '/')
      {
	c = peek ();
	if (Character.digit ((char)c, radix) < 0)
	  throw new ReadError (this,"\"/\" in rational not followed by digit");
	Numeric denominator = readSchemeReal(read(), radix, 'e');
	if (isFloat > 0 || ! (denominator instanceof IntNum))
	  throw new ReadError (this, "invalid fraction");
	return RatNum.make (IntNum.valueOf(str.toString (), radix),
			    (IntNum) denominator);
      }
    
    while (radix == 10
	   && (Character.isLowerCase ((char)c)
	       || Character.isUpperCase ((char)c)))
      {
	String word = readAlphaWord (c);
	if (word.length() == 1 && (c == 'i' || c == 'I'))
	  {
	    imaginary = true;
	    c = read();
	    break;
	  }
	else
	  {
	    Unit u = Unit.lookup (word);
	    if (u == null)
	      throw new ReadError (this, "unknown unit: " + word);
	    int power = readOptionalExponent();
	    if (power != 1)
	      u = Unit.pow (u, power);
	    if (unit == null)
	      unit = u;
	    else
	      unit = Unit.mul(unit, u);
	    c = read();
	    if (exactness != 'e')
	      isFloat = 1;
	  }
      }
    if (c >= 0)
      unread();

    RealNum rnum;
    if (isFloat == 0 && exactness != 'i')
      rnum = IntNum.valueOf(str.toString (), radix);
    else
      {
	rnum = new DFloNum (str.toString ());
	if (exactness == 'e')
	  rnum = rnum.toExact();
      }
    Complex cnum = imaginary ? Complex.make (IntNum.zero(),rnum) : rnum;
    return unit == null ? cnum : Quantity.make (cnum, unit);
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
    int c;
    do
      {
	int next, v;
	c = read();
	switch (c)
	  {
	  case '"':
	    inString = false;
            break;
	  case '\\':
	    switch (c = read())
	      {
	      case 'a':  c = '\007';  break;
	      case 'b':  c = '\b';    break;
	      case 'f':  c = '\f';    break;
	      case 'n':  c = '\n';    break;
	      case 'r':  c = '\r';    break;
	      case 't':  c = '\t';    break;
	      case 'v':  c = '\013';  break;

	      case 'u':
		c = 0;
		for (int i = 4;  --i >= 0; )
		  {
		    v = read ();
		    if (v < 0)
		      throw new EofReadError (this,
					      "premature EOF in \\u escape");
		    v = Character.digit ((char) v, 16);
		    if (v < 0)
		      throw new ReadError (this,
					   "non-hex character following \\u");
		    c = 16 * c + v;
		  }
		break;
	      case '0':  case '1':  case '2':  case '3':
	      case '4':  case '5':  case '6':  case '7':
		c = Character.digit ((char) c, 8);
		if ((next = read ()) >= 0)
		  {
		    if ((v = Character.digit ((char) next, 8)) < 0)
		      unread ();
		    else
		      {
			c = c * 8 + v;
			if ((next = read ()) >= 0)
			  {
			    if ((v = Character.digit ((char) next, 8)) >= 0)
			      c = c * 8 + v;
			    else
			      unread ();
			  }
		      }
		  }
		break;
	      case '\n':  continue;
	      case '"':
	      case '\\':
		break;
	      default:
		if (c < 0)
		  throw new EofReadError (this,
					  "unexpected EOF in string literal");
		throw new ReadError (this, "bad string escape");
	      }
	    obj.append ((char) c);
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

  protected Object readQuote (Symbol func_symbol)
      throws java.io.IOException, ReadError
  {
    return new Pair (func_symbol,
		     new Pair (readSchemeObject (), List.Empty));
  }

  protected void skipWhitespaceAndComments()
      throws java.io.IOException
  {
    int c;
    do
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
		if (c == '\n')
		  break;
	      }
	  }
      } while (Character.isWhitespace ((char) c));
    unread ();
  }

  /** Read a list (possibly improper) of zero or more Scheme forms.
   * Assumes '(' has been read.  Does not read the final ')'.
   */
  protected List readListBody ()
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
	skip ();
	if (c == '.')
	  {
	    int next = peek ();
	    if (next < 0)
	      throw new EofReadError (this, ". followed by EOF");
	    if (Character.isWhitespace((char)next))
	      {
		if (last == null)
		  throw new ReadError (this, ". at start of list");
		//-- Read the cdr for the Pair
		Object cdr = readSchemeObject ();
		skipWhitespaceAndComments();
		if (peek () != ')')
		  throw new ReadError (this, ". OBJECT not followed by )");
		last.cdr = cdr;
		return list;
	      }
	  }

	int line = getLineNumber ();
	int column = getColumnNumber ();

	Object car = readSchemeObject (c);
	PairWithPosition pair = new PairWithPosition (this, car, List.Empty);
	pair.setLine (line, column);
	pair.setFile (getName ());
	if (last == null)
	  list = pair;
	else
	  last.cdr = pair;
	last = pair;
      }
    return list;
  }

  protected List readList ()
       throws java.io.IOException, ReadError
  {
    List list = readListBody ();
    int c = read ();
    if (c < 0)
	throw new EofReadError (this, "unexpected EOF in list");
    return list;
  }

  public Object readSchemeObject ()
      throws java.io.IOException, ReadError
  {
    return readSchemeObject (read ());
  }
  public Object readSchemeObject (int c)
      throws java.io.IOException, ReadError
  {
    for (;;)
      {
	int next;
	while (Character.isWhitespace((char)c))
	  c = read ();
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
	      } while (c!='\n');
            break;
	  case ')':
	    throw new ReadError (this, "An unexpected close paren was read.");
	  case '(':
	    return readList();
	  case '"':
	    return readString();
	  case '\'':
	    return readQuote(Interpreter.quote_sym);
	  case '`':
	    return readQuote(Interpreter.quasiquote_sym);
	  case ',':
	    Symbol func;
	    if (peek()=='@')
	      {
		skip ();
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
		 || (c != '.' && next == '.'))
	      return readSchemeNumber(c, 0, ' ');
	    else
	      return readSymbol(c);
	  case '#':
	    next = read();
	    switch (next)
	      {
	      case '(':
		return readList ().toVector ();
	      case '\\':
		return readCharacter();
	      case '!':
		return readSpecial();
	      case 't':
		return Interpreter.trueObject;
	      case 'f':
		return Interpreter.falseObject;
	      case 'x':
		return readSchemeNumber(16, ' ');
	      case 'b':
		return readSchemeNumber (2, ' ');
	      case 'o':
		return readSchemeNumber (8, ' ');
	      case 'i':
	      case 'e':
		return readSchemeNumber (0, (char) next);
	      case '|':
		int commentNesting = 1;
		do {
		  c = read();
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
		    throw new EofReadError (this,
					    "unexpected eof in #| comment.");
		} while (commentNesting > 0);
		break;
	      default:
		throw new ReadError (this, "An invalid #-construct was read.");
	      }
            break;
	  default:
	    if (Character.isDigit((char)c))
	      return readSchemeNumber(c, 0, ' ');
	    else
	      return readSymbol(c);
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
