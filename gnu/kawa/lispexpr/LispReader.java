package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.mapping.InPort;

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

  protected boolean isDelimiter (char ch)
  {
    return (Character.isWhitespace (ch)
	    || ch == ')' || ch == '(' || ch == '"' || ch == ';');
  }

  protected int skipWhitespaceAndComments()
      throws java.io.IOException
  {
    int c;
    for (;;)
      {
	c = read ();
	if (c < 0)
	  return c;
	if (c == ';')
	  {
	    for (;;)
	      {
		c = read ();
		if (c < 0)
		  return c;
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
    return c;
  }

  /** Read a #|...|#-style comment (which may contain other nested comments).
    * Assumes the initial "#|" has already been read.
    */
  final public void readNestedComment ()
       throws java.io.IOException
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
	  {
	    error("unexpected eof in #| comment.");
	    return;
	  }
      } while (commentNesting > 0);
  }

 /** Read a list (possibly improper) of zero or more Scheme forms.
   * Assumes '(' has been read.  Does not read the final ')'.
   */
  public Object readListBody ()
       throws java.io.IOException, SyntaxException
  {
    return readListBody((int) ')');
  }

  public Object readListBody (int endDelimiter)
       throws java.io.IOException, SyntaxException
  {
    Object last = null;
    Object list = makeNil();

    for (;;)
      {
	int c = skipWhitespaceAndComments();
	if (c < 0)
	    break;
	unread(c);
	if (c == endDelimiter)
	  break;
	int line = port.getLineNumber ();
	int column = port.getColumnNumber ();

	skip();
	if (c == '.')
	  {
	    int next = peek ();
	    if (next < 0)
	      eofError(". followed by EOF");
	    if (isDelimiter((char)next))
	      {
		// if (last == null && pedantic)
		//   error(". at start of list");
		//-- Read the cdr for the Pair
		Object cdr = readObject ();
		c = skipWhitespaceAndComments();
		if (c != endDelimiter)
		  error(". OBJECT not followed by `"+endDelimiter+"'");
		if (c >= 0)
		  unread(c);
		// ( a1 ... an . cdr) creates an n-element list ended by
		// cdr.  If n==0, a reasonable (and common) extension is to
		// interpret this as a 0-element list ended by cdr - i.e.
		// just cdr by itself.
		if (last == null)
		  return cdr;

		setCdr(last, cdr);
		return list;
	      }
	  }

	Object car = readObject (c);
	Object pair = makePair (car, line, column);
	if (last == null)
	  list = pair;
	else
	  setCdr(last, pair);
	last = pair;
      }
    return list;
  }

  protected Object readList ()
    throws java.io.IOException, SyntaxException
  {
    return readList(')');
  }

  protected Object readList (char endDelimiter)
    throws java.io.IOException, SyntaxException
  {
    // FIXME  These casts are not a good idea!
    char saveReadState = ((InPort) port).readState;
    ((InPort) port).readState = endDelimiter;
    int line = port.getLineNumber ();
    int column = port.getColumnNumber ();
    try
      {
	Object list = readListBody((int) endDelimiter);
	int c = read ();
	if (c < 0)
	  error('e', port.getName(), line+1, column+1,
		"unexpected EOF in list starting here");
	return list;
      }
    finally
      {
	((InPort) port).readState = saveReadState;
      }
  }

  public Object readObject ()
      throws java.io.IOException, SyntaxException
  {
    char saveReadState = ((InPort) port).readState;
    ((InPort) port).readState = ' ';
    try
      {
	return readObject (read ());
      }
    finally
      {
	((InPort) port).readState = saveReadState;
      }
  }

  /** Reads a C-style String escape sequence.
   * Assume '\\' has already been read.
   * Return the converted character, or -1 on EOF, or -2 to ignore. */
  protected int readEscape()
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

  protected final int readEscape(int c)
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

  public abstract Object readObject (int c)
    throws java.io.IOException, SyntaxException;

  protected abstract Object makeNil ();
  protected abstract Object makePair (Object car, int line, int column);
  protected abstract void setCdr (Object pair, Object cdr);
}
