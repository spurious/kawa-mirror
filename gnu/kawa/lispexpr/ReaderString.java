// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.mapping.InPort;

public class ReaderString extends ReadTableEntry
{
  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    int startPos = in.tokenBufferLength;
    LineBufferedReader port = in.getPort();
    char saveReadState = '\0';
    int c = ch;
    int prev;
    if (port instanceof InPort)
      {
	saveReadState = ((InPort) port).readState;
	((InPort) port).readState = (char) ch;
      }
    try
      {
	for (;;)
	  {
	    int next;

	    prev = c;

	    // Read next char - inline the common case.
	    if (prev == '\r')
	      {
		c = port.read();
		if (c == '\n')
		  continue;
	      }
	    else if (port.pos < port.limit && prev != '\n')
	      c = port.buffer[port.pos++];
	    else
	      c = port.read();
	    if (c == ch)
	      {
		break;
	      }
	    switch (c)
	      {
	      case '\r':
		in.tokenBufferAppend('\n');
		continue;
	      case '\\':
		if (in instanceof LispReader)
		  c = ((LispReader) in).readEscape();
		else
		  c = port.read();
		if (c == -2)
		  {
		    c = '\n'; // So prev gets set ...
		    continue;
		  }
		/* ... fall through ... */
	      default:
		if (c < 0)
		  in.eofError("unexpected EOF in string literal");
		in.tokenBufferAppend(c);
		break;
	      }
	  }
        /* #ifdef use:java.lang.CharSequence */
	return new String(in.tokenBuffer, startPos,
                          in.tokenBufferLength - startPos);
        /* #else */
        // return new gnu.lists.FString (in.tokenBuffer, startPos,
        //                               in.tokenBufferLength - startPos);
        /* #endif */
      }
    finally
      {
	in.tokenBufferLength = startPos;
	if (port instanceof InPort)
	  ((InPort) port).readState = saveReadState;
      }
  }
}
