package gnu.jemacs.lang;
import gnu.kawa.lispexpr.*;
import gnu.mapping.*;
import gnu.text.*;
import gnu.math.*;
import gnu.expr.*;
import gnu.lists.*;

/** A class to read Emacs Lisp forms (S-expressions). */

public class ELispReader extends LispReader
{
  public ELispReader (InPort port)
  {
    super(port);
  }

  public ELispReader(InPort port, SourceMessages messages)
  {
    super(port, messages);
  }
  
  protected ReadTable getReadTable () { return elispReadTable; }

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

  public static Object readObject(InPort port)
      throws java.io.IOException, SyntaxException
  {
    return (new ScmRead(port)).readObject();
  }

  public static ReadTable elispReadTable;
  static
  {
    elispReadTable = ReadTable.getInitial();
    elispReadTable.set('[', new ReaderVector(']'));
    elispReadTable.set('?', new ELispReadTableEntry('?'));
  }
}

class ELispReadTableEntry extends ReaderDispatchMisc
{
  public ELispReadTableEntry(int code)
  {
    super(code);
  }

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    LispReader reader = (LispReader) in;
    if (code >= 0)
      ch = code;
    switch (ch)
      {
      case '?':
	ch = reader.read();
	if (ch == '\\')
	  {
	    ch = reader.read();
	    if (ch != ' ' && ch >= 0)
	      ch = reader.readEscape(ch);
	  }
	if (ch < 0)
	  {
	    reader.error("unexpected EOF in character literal");
	    ch = '?';
	  }
	return ELisp.getCharacter(ch);
      }
    reader.error("unexpected dispatch character");
    return null;
  }
}
