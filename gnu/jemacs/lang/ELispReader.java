package gnu.jemacs.lang;
import kawa.lang.ScmRead;
import gnu.mapping.*;
import gnu.text.*;
import gnu.math.*;
import gnu.expr.*;
import gnu.kawa.util.*;

/** A class to read Emacs Lisp forms (S-expressions). */

public class ELispReader extends gnu.text.LispReader
{
  public ELispReader (InPort port)
  {
    super(port);
  }

  public ELispReader(InPort port, SourceMessages messages)
  {
    super(port, messages);
  }
  
  protected boolean isDelimiter (char ch)
  {
    return (Character.isWhitespace (ch)
	    || ch == ')' || ch == '(' || ch == '"' || ch == ';'
	    || ch == '[' || ch == ']' || ch == '\'' || ch == '#');
  }

  protected Object makeNil ()
  {
    return LList.Empty;
  }

  protected Object makePair (Object car, int line, int column)
  {
    PairWithPosition pair = new PairWithPosition (port, car, LList.Empty);
    pair.setLine(line + 1, column + 1);
    pair.setFile (port.getName());
    return pair;
  }

  protected void setCdr (Object pair, Object cdr)
  {
    ((Pair) pair).cdr = cdr;
  }

  boolean readAtom0 (int c, StringBuffer str)
       throws java.io.IOException
  {
    char firstChar = (char) c;
    boolean sawBackslash = false;
    if (c < 0 || isDelimiter(firstChar))
      error("no symbol start character");
    for (;;)
      {
	char ch = (char)c;
	if (ch == '\\')
	  {
	    c = read ();
	    if (c < 0)
	      {
		error("EOF after \\ escape");
		str.append("??");
		return true;
	      }
	    ch = (char) c;
	    sawBackslash = true;
	  }
	str.append (ch);
	c = peek();
	if (c < 0 || isDelimiter((char) c))
	  break;
	skip();
      }
    return sawBackslash;
  }

  Object readAtom (int c)
    throws java.io.IOException
  {
    StringBuffer str = new StringBuffer (30);
    boolean sawBackslash = readAtom0(c, str);
    int len = str.length();
    if (! sawBackslash)
      {
	int i = 0;
	char ch = str.charAt(0);
	if (ch == '+' || ch == '-')
	  i++;
	if (i != len)
	  {
	    boolean sawDigits = false;
	    while (i != len && Character.isDigit(str.charAt(i)))
	      {
		i++;
		sawDigits = true;
	      }
	    if (i > 0 && i < len && str.charAt(i) == '.')
	      {
		// Integers can have trailing decimal points.
		if (++i == len)
		  str.setLength(len = --i);
	      }
	    if (i == len && sawDigits) // It is an integer.
	      return IntNum.valueOf(str.toString(), 10);

	    while (i != len && Character.isDigit(str.charAt(i)))
	      {
		i++;
		sawDigits = true;
	      }
	    if (i + 1 < len && ((ch = str.charAt(i)) == 'e' || ch == 'E'))
	      {
		ch = str.charAt(++i);
		sawDigits = false;
		if (ch == '+' || ch == '-')
		  i++;
		while (i != len && Character.isDigit(str.charAt(i)))
		  {
		    i++;
		    sawDigits = true;
		  }
	      }
	    if (i == len && sawDigits)
	      return new DFloNum(str.toString ());
	  }
      }
    return ELisp.getSymbol(str.toString().intern());
  }

  Object readInteger (int base)
    throws java.io.IOException
  {
    StringBuffer str = new StringBuffer (30);
    readAtom0(read(), str);
    return IntNum.valueOf(str.toString(), base);
  }

  boolean charIsInt = true;

  /**
   * Read a ELisp character literal.
   * Assumes the initial '?' and has already been read.
   */
  protected Object readCharacter()
    throws java.io.IOException, SyntaxException
  {
    int c = read();
    if (c < 0)
      {
	error("unexpected EOF in character literal");
	c = '?';
      }
    if (c == '\\')
      c = readEscape();
    if (charIsInt)
      return IntNum.make(c);
    else
      return Char.make((char)c);
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
	int next, v;

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
    ((InPort) port).readState = '[';
     try
       {
	 java.util.Vector vec = new java.util.Vector();
	 for (;;)
	   {
	     int c = skipWhitespaceAndComments();
	     if (c < 0)
	       eofError("unexpected EOF in vector");
	     if (c == ']')
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
	    return readList();
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
	  case '?':
	    return readCharacter();
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
	  case '[':
	    return readVector();
	  case '#':
	    next = read();
	    switch (next)
	      {
	      case ':':
		StringBuffer sbuf = new StringBuffer(30);
		readAtom0(read(), sbuf);
		return new String(sbuf.toString());
	      case 'x':  return readInteger(16);
	      case 'd':  return readInteger(10);
	      case 'o':  return readInteger(8);
	      case 'b':  return readInteger(2);
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
	    return readAtom(c);
	  }
	c = read ();
      }
  }
}
