package gnu.kawa.brl;

// BRLRead.java -- a class to read BRL forms
// Copyright (C) 2001  Bruce R. Lewis and Eaton Vance Management
// See the file COPYING for license terms.

import gnu.kawa.lispexpr.*;
import gnu.text.*;
import gnu.mapping.InPort;
import gnu.lists.UnescapedData;

public class BRLReaderString extends gnu.kawa.lispexpr.ReadTableEntry
{
  public Object read (Lexer in, int ch, int count)
      throws java.io.IOException
    {
	int startPos = in.tokenBufferLength;
	LineBufferedReader port = in.getPort();
	char saveReadState = '\0';
	int c = ch;
	int prev;
	char endch;

	switch((char)ch)
	    {
	    case ']':		// normal BRL case
		endch = '[';
		break;
	    case '}':
		endch = '{';
		break;
	    case '{':
		endch = '}';
		break;
	    case '[':
		endch = ']';
		break;
	    default:
		// By default, symmetric string delimiters
		endch = (char)ch;
		break;
	    }

	if (port instanceof InPort)
	    {
		saveReadState = ((InPort) port).readState;
		((InPort) port).readState = (char) ch;
	    }

	try
	    {
		boolean inString = true;
		while (inString)
		    {
			int next;

			prev = c;

			if (port.pos < port.limit
			    && prev != '\r'
			    && prev != '\n')
			    c = port.buffer[port.pos++];
			else
			    /* If no buffered data, or if port
			       might update lineNumber */
			    c = port.read();
			if (c == endch)
			    {
				if (port.peek() == endch)
				    {
					in.tokenBufferAppend(c);
					port.skip();
				    }
				else
				    {
					inString = false;
					saveReadState = '\n';
				    }
			    }
			else
			    {
				if (c < 0)
				    inString = false;
				else
				    in.tokenBufferAppend(c);
			    }
		    }
	
		String str = new String(in.tokenBuffer, startPos,
					in.tokenBufferLength - startPos);
		return new UnescapedData(str);
	    }
	finally
	    {
		in.tokenBufferLength = startPos;
		if (port instanceof InPort)
		    ((InPort) port).readState = saveReadState;
	    }
    }
}
