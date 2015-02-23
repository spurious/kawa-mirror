// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.kawa.io.InPort;

/** Reader table entry for '<' to treat '[' and ']' as constituents.
 * Lets us use (say) '<char[]>' as a token even if  '[' and ']' are parens.
 * @author Bruce R. Lewis.
 */

public class ReaderTypespec extends ReaderConstituent
{
  public ReaderTypespec() { super(ReadTable.NON_TERMINATING_MACRO); }

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    if (! (in instanceof LispReader))
        return super.read(in, ch, count);
    int endChar = ch == '<' ? '>' : -2;
    LispReader reader = (LispReader) in;
    int startPos = in.tokenBufferLength;
    InPort port = in.getPort();
    ReadTable rtable = ReadTable.getCurrent();
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
                in.tokenBufferAppend(LispReader.TOKEN_ESCAPE_CHAR);
                reader.seenEscapes = true;
	      }
            else if (c == endChar && ! got_open_square)
              {
                reader.readToken('>', rtable);
                break;
              }
	    else
	      {
                int kind;
		if ( (!got_open_square && c == '['
		      && true == (got_open_square = true))
		     || (got_open_square && c == ']'
			 && false == (got_open_square = false))
		     || ((kind = rtable.lookup(c).getKind())
                         == ReadTable.CONSTITUENT
                         || kind == ReadTable.NON_TERMINATING_MACRO))
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
	return reader.handleToken(startPos, rtable);
      }
    finally
      {
	in.tokenBufferLength = startPos;
	if (port instanceof InPort)
	  ((InPort) port).readState = saveReadState;
      }
  }
}
