// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.mapping.InPort;
import gnu.mapping.Values;

public class ReaderParens extends ReadTableEntry
{
  char open;
  char close;
  int kind;

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
	  instance = new ReaderParens(open, close, kind);
	return instance;
      }
    else
      {
	return new ReaderParens(open, close, kind);
      }
  }

  public ReaderParens(char open, char close, int kind)
  {
    this.open = open;
    this.close = close;
    this.kind = kind;
  }

 /** Read a list (possibly improper) of zero or more Scheme forms.
   * Assumes '(' has been read.
   */
  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    return readList((LispReader) in, ch, count, close);
  }

  public static Object readList (LispReader lexer,
				 int ch, int count, int close)
    throws java.io.IOException, SyntaxException
  {
    LineBufferedReader port = lexer.getPort();
    char saveReadState = '\n';
    if (port instanceof InPort)
      {
	saveReadState = ((InPort) port).readState;
	((InPort) port).readState = close == ']' ? '[' : '(';
      }
    int startLine = port.getLineNumber();
    int startColumn = port.getColumnNumber();
    try
      {
	Object last = null;
	Object list = lexer.makeNil();
	boolean sawDot = false;
	boolean sawDotCdr = false;
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
		entry = lexer.getReadTable().lookup(ch);
		int kind = entry == null ? ReadTable.ILLEGAL : entry.getKind();
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
	      entry = lexer.getReadTable().lookup(ch);
	    Object value = lexer.readValues(ch, entry);
	    if (value == Values.empty)
	      continue;
	    if (value == gnu.expr.QuoteExp.voidExp)
	      value = Values.empty;

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
	 if (port instanceof InPort)
	   ((InPort) port).readState = saveReadState;
      }
     
  }
}
