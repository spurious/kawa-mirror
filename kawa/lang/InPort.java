package kawa.lang;
import java.io.*;

/**
 * An InputStream that handles characters (rather than just bytes).
 */

public class InPort extends FilterInputStream implements Printable
{
  protected byte[] buffer;

  int pos;

  int limit;

  String name;

  public InPort (InputStream in)
  {
    super (in);
  }

  public InPort (InputStream in, String name)
  {
    super (in);
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

  public int read ()
       throws java.io.IOException
  {
    if (pos >= limit)
      {
	if (buffer == null)
	  buffer = new byte[100];
	pos = 0;
	limit = 0;
	for (;;)
	  {
	    int ch = in.read ();
	    if (ch == -1)
	      {
		if (pos == limit)
		  return ch;
		break;
	      }
	    if (limit >= buffer.length)
	      {
		byte[] new_buffer = new byte[2 * buffer.length];
		System.arraycopy (buffer, 0, new_buffer, 0, buffer.length);
		buffer = new_buffer;
	      }
	    buffer[limit++] = (byte) ch;
	    if (ch == '\n' || ch == '\r')
	      break;
	  }
      }
    return buffer[pos++];
  }

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
       throws java.io.IOException, SyntaxError
  {
    StringBuffer str = new StringBuffer (30);
    for (;;)
      {
	int c = readChar ();
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
      }
    return Symbol.make (str.toString ());
  }

  /**
   * Read a Scheme character literal.
   * Assumes the initial '#' and '\\' have already been read.
   */
  protected Object readCharacter()
       throws java.io.IOException, SyntaxError  
  {
    int c = readChar ();
    if (c < 0)
      throw new EofReadError (this, "unexpected EOF in character literal");
    int origc = c;
    if (Character.isLowerCase ((char)c) || Character.isUpperCase ((char)c))
      {
	unreadChar ();
	Symbol name = readSymbol ();
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

  protected Object readNumber(int radix)
       throws java.io.IOException, SyntaxError
  {
    int c = readChar();

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
    do
      {
	if (c=='.' || c=='e' || c=='E')
	  isFloat = true;
	str.append ((char) c);
	c = readChar ();
      } while (Character.digit ((char)c, radix) >= 0
	       || c=='.' || c=='e' || c=='E');

    unreadChar();

    if (isFloat)
      return Double.valueOf(str.toString ());
    else
      return Integer.valueOf(str.toString (), radix);
  }

  /**
   * Read a Scheme string literal.
   * Assume we have already skipped the initial '"'.
   */
  protected Object readString()
      throws SyntaxError,
             java.io.IOException
  {
    StringBuffer obj = new StringBuffer ();
    boolean inString = true;
    int c;
    do
      {
	c = readChar();
	switch (c)
	  {
	  case '"':
	    inString = false;
            break;
	  case '\\':
	    switch (c = readChar())
	      {
	      case '"':
	      case '\\':
		obj.append ((char) c);
		break;
	      default:
		if (c < 0)
		  throw new EofReadError (this,
					  "unexpected EOF in string literal");
		obj.append ('\\');
		obj.append ((char)c);
		break;
	      }
            break;
	  default:
	    if (c < 0)
	      throw new EofReadError (this,
				      "unexpected EOF in string literal");
	    obj.append ((char) c);
	    break;
	  }
      } while (inString);
    return obj;
  }

  protected Object readQuote ()
      throws java.io.IOException, SyntaxError
  {
    return new Pair(Interpreter.quote_sym,
		    new Pair(readSchemeObject (), Interpreter.nullObject));
  }

  protected Object readQuasiQuote ()
      throws java.io.IOException, SyntaxError
  {
    return new Pair(Interpreter.quasiquote_sym,
		    new Pair(readSchemeObject (), Interpreter.nullObject));
  }

  protected void skipWhitespaceAndComments()
      throws java.io.IOException
  {
    boolean notAtEnd = true;
    int c;
    do
      {
	c = readChar ();
	if (c < 0)
	  break;
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

  protected List readList ()
      throws java.io.IOException, SyntaxError
  {
     skipWhitespaceAndComments();
     //-- null Primitive
     int c;
     if ((c = peekChar())==')')
       {
         skipChar ();
         return List.Empty;
       }

     //-- Car of the list
     Object car = readSchemeObject ();
     Object cdr = Interpreter.nullObject;
     skipWhitespaceAndComments();

     c = readChar ();
     if (c != ')')
       {
	 int next;
         if (c == '.'
	     && (next = peekChar ()) >= 0
	     && Character.isSpace((char)next))
	   {
	     //-- Read the cdr for the Pair
	     cdr = readSchemeObject ();
	     skipWhitespaceAndComments();
	     if (readChar ()!=')')
	       throw new ReadError (this, "Malformed list.");
	   }
	 else
	   {
	     //-- Read the read of the list
	     unreadChar ();
	     cdr = readList();
	   }           
       }
     return new Pair (car,cdr);
  }

  public Object readSchemeObject ()
      throws java.io.IOException, SyntaxError
  {
    int c;
    while (true)
      {
	c = readChar ();
	while (Character.isSpace((char)c))
	  c = readChar ();
	switch (c)
	  {
	  case -1:
	    return Interpreter.eofObject;
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
	    return readQuote();
	  case '`':
	    return readQuasiQuote();
	  case ',':
	    if (peekChar()=='@')
	      {
		skipChar ();
		return new Pair(Interpreter.unquotesplicing_sym,
				new Pair (readSchemeObject (),
					  Interpreter.nullObject));
	      }
	    else
	      return new Pair(Interpreter.unquote_sym,
			      new Pair (readSchemeObject (),
					Interpreter.nullObject));
	  case '+':
	  case '-':
	    int next = peekChar ();
	    unreadChar ();
	    if (Character.isDigit((char) next))
	      return readNumber(10);
	    else
	      return readSymbol();
	  case '#':
	    switch (readChar()) 
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
		return readNumber(16);
	      case 'b':
		return readNumber (2);
	      case 'o':
		return readNumber (8);
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
	    unreadChar ();
	    if (Character.isDigit((char)c))
	      return readNumber(10);
	    else
	      return readSymbol();
	  }
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
