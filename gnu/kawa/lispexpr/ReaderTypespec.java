// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.mapping.InPort;

/** Reader table entry for '<' to treat '[' and ']' as constituents.
 * Lets us use (say) '<char[]>' as a token even if  '[' and ']' are parens.
 * @author Bruce R. Lewis.
 */

public class ReaderTypespec extends ReadTableEntry
{
  public int getKind()
  {
    return ReadTable.NON_TERMINATING_MACRO;
  }

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    int startPos = in.tokenBufferLength;
    LineBufferedReader port = in.getPort();
    ReadTable rtable =
	(in instanceof LispReader) ?
	((LispReader) in).getReadTable()
	: ReadTable.getCurrent();
    ReadTableEntry entry;
    char saveReadState = '\0';
    in.tokenBufferAppend(ch);
    int c = ch;
    int prev;
    if (port instanceof InPort)
      {
	saveReadState = ((InPort) port).readState;
	((InPort) port).readState = (char) ch;
      }
    try
      {
	boolean got_open_square = false;
	for (;;)
	  {
	    int next;

	    prev = c;

	    if (port.pos < port.limit && prev != '\n')
	      c = port.buffer[port.pos++];
	    else
	      c = port.read();
	    if (c == '\\')
	      {
		if (in instanceof LispReader)
		  c = ((LispReader) in).readEscape();
		else
		  c = port.read();
	      }
	    else
	      {
		if ( (!got_open_square && c == '['
		      && true == (got_open_square = true))
		     || (got_open_square && c == ']'
			 && false == (got_open_square = false))
		     || (null != (entry = rtable.lookup(c))
			 && entry.getKind() == ReadTable.CONSTITUENT))
		  {
		      in.tokenBufferAppend(c);
		      continue;
		  }
		else
		  {
		    in.unread(c);
		    break;
		  }
	      }
	    }
	return (new java.lang.String (in.tokenBuffer, startPos,
				      in.tokenBufferLength - startPos)).intern();
      }
    finally
      {
	in.tokenBufferLength = startPos;
	if (port instanceof InPort)
	  ((InPort) port).readState = saveReadState;
      }
  }
}
