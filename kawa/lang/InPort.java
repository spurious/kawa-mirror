package kawa.lang;
import kawa.math.*;
import java.io.*;

/**
 * An InputStream that handles characters (rather than just bytes).
 */

public class InPort extends FilterInputStream implements Printable
{
  protected byte[] buffer;

  int pos;

  int limit;

  // The current line number (at pos).
  int linenumber;

  // The position that marks the start of the current line, or -1 if unknown.
  int linestart;

  protected int markpos = -1;

  protected int marklimit;

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
    linenumber = 1;
  }

  public InPort (InputStream in, String name)
  {
    this (in);
    this.name = name;
  }

  // For now, this is static.  It should probably be thread-local.
  private static InPort inp = new InPort (System.in, "<stdin>");

  static public InPort inDefault ()
  {
    return inp;
  }

  static public void setInDefault (InPort in)
  {
    inp = in;
  }

  public int getLineNumber ()
  {
    return linenumber;
  }

  public int getColumnNumber ()
  {
    return pos - getLineStart ();
  }

  public void setLineNumber (int linenumber)
  {
    this.linenumber = linenumber;
  }

  public boolean markSupported ()
  {
    return true;
  }

  private final int getLineStart ()
  {
    if (linestart < 0)
      {
	linestart = pos;
	while (linestart > 0 && buffer[linestart - 1] != '\n')
	  linestart--;
      }
    return linestart;
  }

  public synchronized void mark(int read_limit)
  {
    marklimit = read_limit;
    markpos = pos;
  }

  public void reset ()  throws IOException
  {
    if (markpos < 0)
      throw new IOException("Resetting to invalid mark");
    if (markpos < getLineStart ())
      {
	for (pos = linestart; pos > markpos; )
	  {
	    --pos;
	    if (buffer[pos] == '\n')
	      linenumber--;
	  }
	linestart = -1;
      }
    pos = markpos;
  }

  public int available () throws java.io.IOException
  {
    int avail;
    try
      {
	avail = in.available ();
      }
    catch (java.io.IOException ex)
      {
	avail = 0;
      }
    return (limit - pos) + avail;
  }

  /** Read a byte. */

  public int read ()
       throws java.io.IOException
  {
    if (pos >= limit)
      {
	// Calculate how much to save from the existing buffer.
	// This should be the last line starting before both markpos and pos.
	int save_start = pos;
	if (markpos >= 0 && markpos < pos)
	  {
	    if (pos - markpos > marklimit)
	      markpos = -1;
	    else
	      save_start = markpos;
	  }
	if (getLineStart () > save_start)
	  {
	    while (save_start > 0 && buffer[save_start - 1] != '\n')
	      save_start--;
	  }
	else
	  save_start = linestart;

	int avail;
	try
	  {
	    avail = in.available ();
	  }
	catch (java.io.IOException ex)
	  {
	    avail = 0;
	  }
	if (buffer == null)
	  buffer = new byte[avail >= 1024 ? 1024 : 256];
	else
	  {
	    byte[] new_buffer;
	    int copy_size = limit - save_start;
	    if (copy_size >= buffer.length)
	      new_buffer = new byte [2 * buffer.length];
	    else
	      {
		new_buffer = buffer;
		if (save_start == 0)
		  copy_size = 0;
	      }
	    System.arraycopy (buffer, save_start, new_buffer, 0, copy_size);
	    buffer = new_buffer;
	  }

	pos -= save_start;
	if (markpos >= 0)
	  markpos -= save_start;
	linestart -= save_start;

	int to_read = buffer.length - pos;
	if (to_read > avail)
	  to_read = avail > 0 ? avail : 1;
	to_read = in.read (buffer, pos, to_read);
	if (to_read <= 0)
	  {
	    limit = pos;
	    return -1;
	  }
	limit = pos + to_read;
      }
    int ch = buffer[pos++];
    if (ch == '\n')
      {
	linestart = pos;
	linenumber++;
      }
    return ch;
  }

  /** Read a (possibly multi-byte) character.
   * Currently, we assume the external representation is UTF-8. */

  public int readChar ()
       throws java.io.IOException
  {
    int byte0 = read ();
    if (byte0 < 128) // handles EOF
      return byte0;
    int byte1 = read ();
    if ((byte1 & 0xC80) == 0x80)
      {
	if ((byte0 & 0xE0) == 0xC0)
	  return ((byte0 & 0x3F) << 6) | (byte1 & 0x3F);
	int byte2 = read ();
	if ((byte2 & 0xC0) == 0x80)
	  {
	    if ((byte0 & 0xF0) == 0xE0)
	      return ((byte0 & 0x1F) << 12)
		| ((byte1 & 0x3F) << 6) | (byte2 & 0x3F);
	    int byte3 = read ();
	    if ((byte3 & 0xC0) == 0x80)
	      {
		if ((byte0 & 0xF8) == 0xF0)
		  return ((byte0 & 0x0F) << 18) | ((byte1 & 0x3F) << 12)
		    | ((byte2 & 0x3F) << 6) | (byte3 & 0x3F);
		int byte4 = read ();
		if ((byte4 & 0xC0) == 0x80)
		  {
		    if ((byte4 & 0xFC) == 0xF8)
		      return ((byte0 & 0x03) << 24) | ((byte1 & 0x3F) << 18)
			| ((byte2 & 0x3F) << 12) | ((byte3 & 0x3F) << 6)
			| (byte4 & 0x3F);
		    int byte5 = read ();
		    if ((byte5 & 0xC0) == 0x80)
		      {
			if ((byte5 & 0xFE) == 0xFC)
			  return ((byte0 & 0x1) << 30) | ((byte1 & 0x3F) << 24)
			    | ((byte2 & 0x3F) << 18) | ((byte3 & 0x3F) << 12)
			    | ((byte4 & 0x3F) << 6) | (byte5 & 0x3F);
		      }
		  }
	      }
	  }
      }
    // Should probably raise exception (malformed UTF multi-byte).  FIXME.
    return -1;
  }

  public void unreadChar ()
  {
    for (;;)
      {
	byte ch = buffer[--pos];
	if (ch == '\n')
	  {
	    linestart = -1;
	    linenumber--;
	  }
	if ((ch & 0xC0) != 0x80)
	  return;
      }
  }

  public int peekChar ()
       throws java.io.IOException
  {
    int ch = readChar ();
    if (ch >= 0)
      unreadChar ();
    return ch;
  }

  public void skipChar ()
       throws java.io.IOException
  {
    readChar ();
  }

  Symbol readSymbol ()
       throws java.io.IOException, ReadError
  {
    return readSymbol (readChar ());
  }

  Symbol readSymbol (int c)
       throws java.io.IOException, ReadError
  {
    StringBuffer str = new StringBuffer (30);
    for (;;)
      {
	if (c < 0)
	  break;
	char ch = (char)c;
	if (Character.isSpace (ch)
	    || ch == ')' || ch == '(' || ch == '"' || ch == ';')
	  {
	    unreadChar ();
	    break;
	  }
	if (ch == '\\')
	  {
	    c = readChar ();
	    if (c < 0)
	      break;  // Error
	    ch = (char) c;
	  }
	else
	  ch = Character.toLowerCase (ch);
	str.append (ch);
	c = readChar ();
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
    int c = readChar ();
    if (c < 0)
      throw new EofReadError (this, "unexpected EOF in character literal");
    int origc = c;
    if (Character.isLowerCase ((char)c) || Character.isUpperCase ((char)c))
      {
	Symbol name = readSymbol (c);
        int i = Char.charNames.length; 
        for ( ; ; ) {
           if (--i < 0) {
              break;
           }
           if (Char.charNames[i] == name) {
              c = Char.charNameValues[i];
             break;
           }
        }
        if (i<0) {
           if (name.toString().length()>1) {
              throw new ReadError (
                 this,
                 "unknown character name: " + name.toString()
              );
           } else {
              c = origc;
           }
        }
      }
    return Char.make((char)c);
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
	    unreadChar();
	    break;
	  }
	c = readChar();
      }
    return str.toString();
  }

  /** Read an optional signed int.
   * If there is no int in the input stream, return 1.
   */

  int readOptionalExponent()
       throws java.io.IOException, ReadError
  {
    int sign = readChar();
    boolean neg = false;
    int c;
    if (sign == '+' || sign == '-')
      c = readChar();
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
	    c = readChar();
	    int d = Character.digit ((char)c, 10);
	    if (d < 0)
	      break;
	    value = 10 * value + d;
	  }
      }
    if (c >= 0)
      unreadChar();
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
    return readSchemeNumber (readChar(), radix, exactness);
  }

  public Numeric readSchemeNumber(int c, int radix, char exactness)
       throws java.io.IOException, ReadError
  {
    while (c == '#')
      {
	c = readChar();
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
	c = readChar ();
      }
    if (radix == 0)
      radix = 10;
    Quantity num = readSchemeReal (c, radix, exactness);
    c = readChar();
    if (c == '@')
      {
	Quantity im = readSchemeReal (readChar(), radix, exactness);
	return Quantity.add (num, Quantity.mul (im, Complex.imOne), 1);
      }
    else if (c == '+')
      {
	Quantity im = readSchemeReal (readChar(), radix, exactness);
	return Quantity.add (num, im, 1);
      }
    else if (c == '-')
      {
	Quantity im = readSchemeReal (readChar(), radix, exactness);
	return Quantity.add (num, im, -1);
      }
    else if (Character.isLowerCase ((char)c)
	     || Character.isUpperCase ((char)c))
      {
	throw new ReadError (this,
			     "unexpected latter '"+((char)c)+"'after number");
      }
    else if (c >= 0)
      unreadChar();
    return num;
  }

  /* This actually also handles a real followed by 'i' - for now. */
  public Quantity readSchemeReal(int c, int radix, char exactness)
       throws java.io.IOException, ReadError
  {
    boolean isFloat = false;
    StringBuffer str = new StringBuffer(20);
    if (c=='+')
      {
	c = readChar();
      }
    else if (c=='-')
      {
	str.append ((char) c);
	c = readChar ();
      }

    int digits = 0;
    Unit unit = null;
    boolean imaginary = false;
    for (;;)
      {
	if (Character.digit ((char)c, radix) >= 0)
	  digits++;
	else if (radix == 10
		 && (Character.isLowerCase ((char)c)
		     || Character.isUpperCase ((char)c)))
	  {
	    String word = readAlphaWord (c);
	    if (word.length() == 1)
	      c = Character.toLowerCase ((char)c);
	    else
	      c = 0;
	    int next;
	    if ((c == 'e' || c == 's' || c == 'f' || c == 'd' || c == 'l')
		&& ((next = peekChar()) == '+' || next == '-'
		    || Character.digit((char)next, 10) >= 10))
	      {
		c = 'e';
		isFloat = true;
	      }
	    else if (c == 'i')
	      {
		imaginary = true;
		c = readChar();
		break;
	      }
	    else
	      {
		unit = Unit.lookup (word);
		if (unit == null)
		  throw new ReadError (this, "unknown unit: " + word);
		int power = readOptionalExponent();
		if (power != 1)
		  unit = Unit.pow (unit, power);
		c = readChar();
		if (exactness != 'e')
		  isFloat = true;
		break;
	      }
	  }
	else if (c == '.')
	  isFloat = true;
	else // catches EOF
	  break;
	str.append ((char) c);
	c = readChar ();
      }
    if (digits == 0)
      throw new ReadError (this, "number constant with no digits");
    if (c == '/')
      {
	c = peekChar ();
	if (Character.digit ((char)c, radix) < 0)
	  throw new ReadError (this,"\"/\" in rational not followed by digit");
	Numeric denominator = readSchemeReal(readChar(), radix, 'e');
	if (isFloat || ! (denominator instanceof IntNum))
	  throw new ReadError (this, "invalid fraction");
	return RatNum.make (IntNum.valueOf(str.toString (), radix),
			    (IntNum) denominator);
      }
    if (c >= 0)
      unreadChar();

    RealNum rnum;
    if (!isFloat && exactness != 'i')
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
	c = readChar();
	switch (c)
	  {
	  case '"':
	    inString = false;
            break;
	  case '\\':
	    switch (c = readChar())
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
		    v = readChar ();
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
		if ((next = readChar ()) >= 0)
		  {
		    if ((v = Character.digit ((char) next, 8)) < 0)
		      unreadChar ();
		    else
		      {
			c = c * 8 + v;
			if ((next = readChar ()) >= 0)
			  {
			    if ((v = Character.digit ((char) next, 8)) >= 0)
			      c = c * 8 + v;
			    else
			      unreadChar ();
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
		     new Pair (readSchemeObject (),
			       Interpreter.nullObject));
  }

  protected void skipWhitespaceAndComments()
      throws java.io.IOException
  {
    int c;
    do
      {
	c = readChar ();
	if (c < 0)
	  return;
	if (c == ';')
	  {
	    for (;;)
	      {
		c = readChar ();
		if (c < 0)
		  return;
		if (c == '\n')
		  break;
	      }
	  }
      } while (Character.isSpace ((char) c));
    unreadChar ();
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
	int c = peekChar ();
	if (c == ')' || c < 0)
	  break;
	skipChar ();
	if (c == '.')
	  {
	    int next = peekChar ();
	    if (next < 0)
	      throw new EofReadError (this, ". followed by EOF");
	    if (Character.isSpace((char)next))
	      {
		if (last == null)
		  throw new ReadError (this, ". at start of list");
		//-- Read the cdr for the Pair
		Object cdr = readSchemeObject ();
		skipWhitespaceAndComments();
		if (peekChar () != ')')
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
    int c = readChar ();
    if (c < 0)
	throw new EofReadError (this, "unexpected EOF in list");
    return list;
  }

  public Object readSchemeObject ()
      throws java.io.IOException, ReadError
  {
    return readSchemeObject (readChar ());
  }
  public Object readSchemeObject (int c)
      throws java.io.IOException, ReadError
  {
    for (;;)
      {
	int next;
	while (Character.isSpace((char)c))
	  c = readChar ();
	switch (c)
	  {
	  case -1:
	    return Sequence.eofValue;
	  case ';':
	    do
	      {
		c = readChar();
		if (c < 0) // EOF
		  return Interpreter.nullObject;
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
	    if (peekChar()=='@')
	      {
		skipChar ();
		func = Interpreter.unquotesplicing_sym;
	      }
	    else
	      func = Interpreter.unquote_sym;
	    return readQuote (func);
	  case '.':
	  case '+':
	  case '-':
	    next = peekChar ();
	    if (Character.isDigit((char) next)
		 || (c != '.' && next == '.'))
	      return readSchemeNumber(c, 0, ' ');
	    else
	      return readSymbol(c);
	  case '#':
	    next = readChar();
	    switch (next)
	      {
	      case '(':
		return readList ().toVector ();
	      case '\\':
		return readCharacter();
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
		boolean notAtEnd = true;
		do {
		  c = readChar();
		  if (c < 0)
		    throw new EofReadError (this,
					    "unexpected eof in #| comment.");

		  if (c=='|' && readChar()=='#')
		    notAtEnd = false;
		} while (notAtEnd);
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
	c = readChar ();
      }
  }

  public void print(java.io.PrintStream ps)
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
