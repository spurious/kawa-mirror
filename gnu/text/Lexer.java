// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;
import gnu.math.*;
import java.io.*;

/**
 * Framework for implementing lexical scanners.
 * @author	Per Bothner
 */

public class Lexer extends Reader
{
  protected LineBufferedReader port;

  public Lexer(LineBufferedReader port)
  {
    this.port = port;
  }

  public Lexer(LineBufferedReader port, SourceMessages messages)
  {
    this.port = port;
    this.messages = messages;
  }

  public void close() throws java.io.IOException
  {
    port.close();
  }

  public int read() throws java.io.IOException
  {
    return port.read();
  }

  public int read(char[] buf, int offset, int length)
    throws java.io.IOException
  {
    return port.read(buf, offset, length);
  }

  public void unread(int ch) throws java.io.IOException
  {
    port.unread();
  }

  public int peek() throws java.io.IOException
  {
    return port.peek();
  }

  public void skip() throws java.io.IOException
  {
    port.skip();
  }

  protected void unread() throws java.io.IOException
  {
    port.unread();
  }

  protected void unread_quick() throws java.io.IOException
  {
    port.unread_quick();
  }

  protected void skip_quick() throws java.io.IOException
  {
    port.skip_quick();
  }

  SourceMessages messages = null;

  public SourceMessages getMessages () { return messages; }
  public void setMessages (SourceMessages messages)
  { this.messages = messages; }

  /** Returns true if any error were seen.  Prints and clears the errors.
   * @param out where to write the error message to
   * @param max maximum number of messages to print (can be 0) */
  public boolean checkErrors(PrintWriter out, int max)
  {
    return messages != null && messages.checkErrors(out, max);
  }

  public SourceError getErrors()
  { return messages == null ? null : messages.getErrors(); }

  public boolean seenErrors()
  { return messages != null && messages.seenErrors(); }

  public void clearErrors() { if (messages != null) messages.clearErrors(); }

  public void error(char severity, String filename, int line, int column,
		    String message)
  {
    if (messages == null)
      messages = new SourceMessages();
    messages.error(severity, filename, line, column, message);
  }

  public void error(char severity, String message)
  {
    int line = port.getLineNumber();
    int column = port.getColumnNumber();
    error(severity, port.getName(), line + 1, column >= 0 ? column + 1 : 0,
	  message);
  }

  public void error(String message)
  {
    error('e', message);
  }

  public void fatal(String message) throws SyntaxException
  {
    error('f', message);
    throw new SyntaxException(messages);
  }

  public void eofError(String msg) throws SyntaxException
  {
    fatal(msg);
  }

  /** Read an optional signed integer.
   * If there is no integer in the input stream, return 1.
   * For excessively large exponents, return Integer.MIN_VALUE
   * or Integer.MAX_VALUE.
   */
  public int readOptionalExponent()
       throws java.io.IOException
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
	  error("exponent sign not followed by digit");
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
      unread(c);
    if (sign == '-')
      value = -value;
    if (overflow)
      return sign == '-' ? Integer.MIN_VALUE : Integer.MAX_VALUE;
    return value;
  }

  /** Read digits, up to the first non-digit or the buffer limit
    * @return the digits seen as a non-negative long, or -1 on overflow
    */
  public static long readDigitsInBuffer (LineBufferedReader port, int radix)
  {
    long ival = 0;
    boolean overflow = false;
    long max_val = Long.MAX_VALUE / radix;
    int i = port.pos;
    if (i >= port.limit)
      return 0;
    for (;;)
      {
	char c = port.buffer[i];
	int dval = Character.digit(c, radix);
	if (dval < 0)
	  break;
	if (ival > max_val)
	  overflow = true;
	else
	  ival = ival * radix + dval;
	if (ival < 0)
	  overflow = true;
	if (++i >= port.limit)
	  break;
      }
    port.pos = i;
    return overflow ? -1 : ival;
  }

}
