package gnu.ecmascript;
import kawa.lang.*;

/**
  * Reads EcmaScript token from a InPort.
  */

public class Lexer
{
  InPort port;
  private boolean prevWasCR = false;

  public Lexer (InPort port)
  {
    this.port = port;
  }

  public final static kawa.lang.Char lparenToken = kawa.lang.Char.make('(');
  public final static kawa.lang.Char rparenToken = kawa.lang.Char.make(')');
  public final static kawa.lang.Char lbraceToken = kawa.lang.Char.make('{');
  public final static kawa.lang.Char rbraceToken = kawa.lang.Char.make('}');
  public final static kawa.lang.Char dotToken = kawa.lang.Char.make('.');
  public final static kawa.lang.Char condToken = kawa.lang.Char.make('?');
  public final static kawa.lang.Char commaToken = kawa.lang.Char.make(',');
  public final static kawa.lang.Char colonToken = kawa.lang.Char.make(':');
  public final static kawa.lang.Char equalToken = kawa.lang.Char.make('=');
  public final static kawa.lang.Char semicolonToken = kawa.lang.Char.make(';');
  public final static Object eolToken = kawa.lang.Char.make('\n');
  public final static Object eofToken = Sequence.eofValue;

  /*
  static java.util.Hashtable reserved = new java.util.Hashtable();
  private static void reserve(Reserved op) { reserve.put(op.name, op); }
  static {
    reserve(Reserved.plusOp);
    reserve(Reserved.minusOp);
    reserve(Reserved.timesOp);
  }
  */

  public Double getNumericLiteral (int c)
    throws java.io.IOException, kawa.lang.ReadError
  {
    int radix = 10;
    if (c == '0')
      {
	c = port.read();
	if (c == 'x' || c == 'X')
	  {
	    radix = 16;
	    c = port.read();
	  }
	else if (c == '.' || c == 'e' || c == 'E') ;
	else
	  radix = 8;
      }
    int i = port.pos;
    if (c >= 0)
      i--;   // Reset to position before current char c.
    port.pos = i;
    long ival = port.readDigits(radix);
    boolean digit_seen = port.pos > i;
    if (digit_seen && port.pos < port.limit)
      {
	c = port.buffer[port.pos];
	if (! Character.isLetterOrDigit((char) c) && c != '.')
	  {
	    double dval;
	    if (ival >= 0)
	      dval = (double) ival;
	    else // FIXME - do we want to use gnu.math??
	      dval = gnu.math.IntNum.valueOf(port.buffer, i, port.pos - i,
					     radix, false).doubleValue();
	    return new Double(dval);
	  }
      }
    if (radix != 10)
      throw new ReadError (port, "invalid character in non-decimal number");
    StringBuffer str = new StringBuffer (20);
    if (digit_seen)
      str.append(port.buffer, i, port.pos - i);

   /* location of decimal point in str.  */
    int point_loc = -1;
    int exp = 0;
    boolean exp_seen = false;
    for (;;)
      {
	c = port.read ();
	if (Character.digit ((char)c, radix) >= 0)
	  {
	    digit_seen = true;
	    str.append ((char) c);
	    continue;
	  }
	switch (c)
	  {
	  case '.':
	    if (point_loc >= 0)
	      throw new ReadError (port, "duplicate '.' in number");
	    point_loc = str.length ();
	    str.append ('.');
	    continue;
	  case 'e':  case 'E':
	    int next;
	    if (radix != 10 || !((next = port.peek ()) == '+' || next == '-'
				 || Character.digit ((char)next, 10) >= 0))
	      break;
	    if (!digit_seen)
	      throw new ReadError(port, "mantissa with no digits");
	    exp = port.readOptionalExponent();
	    exp_seen = true;
	    c = port.read();
	    break;
	  }
	break;
      }

    if (c >= 0)
      port.unread ();

    if (exp != 0)
      {
	str.append('e');
	str.append(exp);
      }
    return new Double(str.toString ());
  }

  public static String getStringLiteral (InPort port, char quote)
    throws java.io.IOException, kawa.lang.ReadError
  {
    int i = port.pos;
    int start = i;
    int limit = port.limit;
    char[] buffer = port.buffer;
    char c;
    for ( ; i < limit; i++)
      {
	c = buffer[i];
	if (c == quote)
	  {
	    port.pos = i+1;
	    return new String(buffer, start, i - start);
	  }
	if (c == '\\' || c == '\n' || c == '\r')
	  break;
      }
    port.pos = i;
    StringBuffer sbuf = new StringBuffer();
    sbuf.append(buffer, start, i - start);
    for (;;)
      {
	int ch = port.read();
	if (ch == quote)
	  return sbuf.toString();
	if (ch < 0)
	  throw new EofReadError(port, "unterminated string literal");
	if (ch == '\n' || ch == '\r')
	  throw new ReadError(port, "string literal not terminated before end of line");
	if (ch == '\\')
	  {
	    ch = port.read();
	    int val;
	    switch (ch)
	      {
	      case -1:
		throw new EofReadError(port, "eof following '\\' in string");
	      case '\n': case '\r':
		throw new ReadError(port, "line terminator following '\\' in string");
	      case '\'':  case '\"':  case '\\':
		break;
	      case 'b':  ch = '\b';  break;
	      case 't':  ch = '\t';  break;
	      case 'n':  ch = '\n';  break;
	      case 'f':  ch = '\f';  break;
	      case 'r':  ch = '\r';  break;
	      case 'x':  case 'u':
		val = 0;
		for (i = ch == 'x' ? 2 : 4;  --i >= 0; )
		  {
		    int d = port.read();
		    if (d < 0)
		      throw new EofReadError(port, "eof following '\\"
					     +((char)ch)+"' in string");
		    d = Character.forDigit((char) d, 16);
		    if (d < 0)
		      throw new ReadError(port, "invalid char following '\\"
					  +((char)ch)+"' in string");
		    val = 16 * val + d;
		  }
		ch = val;
		break;
	      default:
		if (ch < '0' || ch > '7')
		  break;
		val = 0;
		for (i = 3;  --i >= 0; )
		  {
		    int d = port.read();
		    if (d < 0)
		      throw new EofReadError(port, "eof in octal escape in string literal");
		    d = Character.forDigit((char) d, 8);
		    if (d < 0)
		      {
			port.unread_quick();
			break;
		      }
		    val = 8 * val + d;
		  }
		ch = val;
		break;
		
	      }
	  }
	sbuf.append((char) ch);
      }
  }

  public static String getIdentifier (InPort port, int ch) 
    throws java.io.IOException
  {
    int i = port.pos;
    int start = i - 1;
    int limit = port.limit;
    char[] buffer = port.buffer;
    while (i < limit && Character.isJavaIdentifierPart(buffer[i]))
      i++;
    port.pos = i;
    if (i < limit)
      return new String(buffer, start, i - start);
    StringBuffer sbuf = new StringBuffer();
    sbuf.append(buffer, start, i - start);
    for (;;)
      {
	ch = port.read();
	if (ch < 0)
	  break;
	if (Character.isJavaIdentifierPart((char) ch))
	  sbuf.append((char) ch);
	else
	  {
	    port.unread_quick();
	    break;
	  }
      }
    return sbuf.toString();
  }

  /**
    * Returns the next token.
    * Returns: <dl>
    * <dt>end-of-file<dd>Sequence.eofValue
    * <dt>end-of-line>dd>eolToken
    * <dt>reserved word<dd> ???
    * <dt>identifier><dd>a java.lang.String
    * <dt>punctuator<dd> ???
    * </dl>
    * Literals are returned a QuoteExp objects,  Specifically:
    * <dl>
    * <dt>numeric literal<dd>a QuoteExp of a java.lang.Double value
    * <dt>boolean literal<dd>a QuoteExp of java.lang.Boolean.TRUE or FALSE
    * <dt>null literal<dd>a QuoteExp whose value is null
    * <dt>string literal<dd>a QuoteExp whose value is a String
    * </dl>
    */

  public Object getToken()
    throws java.io.IOException, kawa.lang.ReadError
  {
    int ch = port.read();
    for (;;)
      {
	if (ch < 0)
	  return eofToken;
	if (! Character.isWhitespace((char) ch))
	  break;
	if (ch == '\r')
	  {
	    prevWasCR = true;
	    return eolToken;
	  }
	if (ch == '\n' && ! prevWasCR)
	  return eolToken;
	prevWasCR = false;
	ch = port.read();
      }

    switch (ch)
      {
      case '.':
	ch = port.peek();
	if (ch >= '0' && ch  <= '9')
	  return new QuoteExp(getNumericLiteral('.'));
	return dotToken;
      case '0':  case '1':  case '2':  case '3':  case '4':
      case '5':  case '6':  case '7':  case '8':  case '9':
	return new QuoteExp(getNumericLiteral(ch));
      case '\'':  case '\"':
	return new QuoteExp(getStringLiteral(port, (char) ch));
      case '(':  return lparenToken;
      case ')':  return rparenToken;
      case '{':  return lbraceToken;
      case '}':  return rbraceToken;
      case '?':  return condToken;
      case ':':  return colonToken;
      case ';':  return semicolonToken;
      case ',':  return commaToken;
      case '=':  return equalToken;
      case '+':
	return Reserved.plusOp;
      case '-':
	return Reserved.minusOp;
      case '*':
	return Reserved.timesOp;
      case '<':
	ch = port.peek();
	switch (ch)
	  {
	  case '<':
	    port.skip_quick();
	    return Reserved.lshiftOp;
	  default:
	    return Reserved.lessOp;
	  }
      }
    if (Character.isJavaIdentifierStart((char) ch))
      {
	String word = getIdentifier(port, ch).intern();
	if (word == "null")
	  return new QuoteExp(null);
	if (word == "false")
	  return new QuoteExp(java.lang.Boolean.FALSE);
	if (word == "true")
	  return new QuoteExp(java.lang.Boolean.TRUE);
	// FIXME handle reserved identifiers
	return word;
      }
    return kawa.lang.Char.make((char) ch);
  }

  public static Object getToken(InPort inp)
    throws java.io.IOException, kawa.lang.ReadError
  {
    return new Lexer(inp).getToken();
  }

  public static void main(String[] args)
  {
    InPort inp = InPort.inDefault();
    Lexer reader = new Lexer(inp);
    for (;;)
      {
	try
	  {
	    Object token = reader.getToken();
	    OutPort out = OutPort.outDefault();
	    out.print("token:");
	    SFormat.print(token, out);
	    out.println(" [class:"+token.getClass()+"]");
	    if (token == Sequence.eofValue)
	      break;
	  }
	catch (Exception ex)
	  {
	    System.err.println("caught exception:"+ex);
	    return;
	  }
      }
  }
}
