// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.mapping.Values;

public class ReaderParens extends ReadTableEntry
{
  char open;
  char close;
  int kind;
  Object command;

  public int getKind()
  {
    return kind;
  }

  private static ReaderParens instance;

  public static ReaderParens getInstance(char open, char close)
  {
    return getInstance(open, close, ReadTable.TERMINATING_MACRO);
  }

  public static ReaderParens getInstance(char open, char close, int kind)
  {
    if (open == '(' && close == ')' && kind == ReadTable.TERMINATING_MACRO)
      {
	if (instance == null)
	  instance = new ReaderParens(open, close, kind, null);
	return instance;
      }
    else
      {
	return new ReaderParens(open, close, kind, null);
      }
  }

  public static ReaderParens getInstance(char open, char close, int kind, Object command)
  {
    if (command == null)
      return getInstance(open, close, kind);
    else
      return new ReaderParens(open, close, kind, command);
  }

  public ReaderParens(char open, char close, int kind, Object command)
  {
    this.open = open;
    this.close = close;
    this.kind = kind;
    this.command = command;
  }

 /** Read a list (possibly improper) of zero or more Scheme forms.
   * Assumes '(' has been read.
   */
  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    Object r = readList((LispReader) in, ch, count, close);
    if (command != null)
      {
        LineBufferedReader port = in.getPort();
        int startLine = port.getLineNumber();
        int startColumn = port.getColumnNumber();
        Object p = ((LispReader) in).makePair(command, startLine, startColumn);
        ((LispReader) in).setCdr(p, r);
        r = p;
      }
    return r;
  }

  public static Object readList (LispReader lexer,
				 int ch, int count, int close)
    throws java.io.IOException, SyntaxException
  {
    LineBufferedReader port = lexer.getPort();
    char saveReadState = lexer.pushNesting(close == ']' ? '[' : '(');
    int startLine = port.getLineNumber();
    int startColumn = port.getColumnNumber();
    try
      {
	Object last = null;
	Object list = lexer.makeNil();
	boolean sawDot = false;
	boolean sawDotCdr = false;
	ReadTable readTable = ReadTable.getCurrent();
	for (;;)
	  {
	    int line = port.getLineNumber();
	    int column = port.getColumnNumber();
	    ch = port.read();
	    if (ch == close)
	      break;
	    if (ch < 0)
	       lexer.eofError("unexpected EOF in list starting here",
			      startLine + 1, startColumn);
	    ReadTableEntry entry;
	    if (ch == '.')
	      {
		ch = port.peek();
		entry = readTable.lookup(ch);
		int kind = entry.getKind();
		if (kind == ReadTable.WHITESPACE
		    || kind == ReadTable.TERMINATING_MACRO
		    || kind == ReadTable.ILLEGAL)
		  {
		    port.skip();
		    column++;
		    if (ch == close)
		      {
			lexer.error("unexpected '"
				    + ((char) close) + "' after '.'");
			break;
		      }
		    if (ch < 0)
		      lexer.eofError("unexpected EOF in list starting here",
				     startLine + 1, startColumn);
		    if (sawDot)
		      {
			lexer.error("multiple '.' in list");
			sawDotCdr = false;
			list = lexer.makeNil();
			last = null;
		      }
		    sawDot = true;
		  }
		else
		  {
		    // Treat '.' as start of token.
		    ch = '.';
		    entry = ReadTableEntry.getConstituentInstance();
		  }
	      }
	    else
	      entry = readTable.lookup(ch);
	    Object value = lexer.readValues(ch, entry, readTable);
	    if (value == Values.empty)
	      continue;
            value = lexer.handlePostfix(value, readTable, line, column);

	    // ( a1 ... an . cdr) creates an n-element list ended by
	    // cdr.  If n==0, a reasonable (and common) extension is to
	    // interpret this as a 0-element list ended by cdr - i.e.
	    // just cdr by itself.

	    if (sawDotCdr)
	      {
		lexer.error("multiple values after '.'");
		last = null;
		list = lexer.makeNil();
		sawDotCdr = false;
		continue;
	      }
	    else if (sawDot)
	      {
		sawDotCdr = true;
	      }
	    else
	      {
		if (last == null)
		  {
		    line = startLine;
		    column = startColumn-1;
		  }
		value = lexer.makePair(value, line, column);
	      }
	    if (last == null)
	      list = value;
	    else
	      lexer.setCdr(last, value);
	    last = value;
	  }
	return list;
      }
    finally
      {
	lexer.popNesting(saveReadState);
      }
     
  }
}
