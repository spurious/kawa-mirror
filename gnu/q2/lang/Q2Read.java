package gnu.q2.lang;
import gnu.kawa.lispexpr.*;
import gnu.expr.QuoteExp;
import gnu.text.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.Keyword;
import gnu.kawa.xml.MakeAttribute;

/** A class to read Scheme forms (S-expressions). */

public class Q2Read extends LispReader
{
  void init()
  {
    initialColonIsKeyword = false;
    ((InPort) port).readState = ' ';
  }

  public Q2Read(InPort port)
  {
    super(port);
    init();
  }
  
  public Q2Read(InPort port, SourceMessages messages)
  {
    super(port, messages);
    init();
  }
  
  int skipIndentation ()
      throws java.io.IOException, SyntaxException
  {
    int numTabs = 0, numSpaces = 0;
    int ch = port.read();
    while (ch == '\t')
      {
	numTabs++;
	ch = port.read();
      }
    while (ch == ' ')
      {
	numSpaces++;
	ch = port.read();
      }
    if (ch < 0)
      return -1;
    port.unread();
    return (numTabs << 16) + numSpaces;
  }

  boolean singleLine()
  {
    return interactive && nesting == 0;
  }

  public Object readCommand ()
      throws java.io.IOException, SyntaxException
  {
    return readCommand(false);
  }

  public Object readCommand (boolean forceList)
      throws java.io.IOException, SyntaxException
  {
    int line = port.getLineNumber();
    int startColumn = port.getColumnNumber();
    int lastColumn = startColumn;
    Object obj = LList.Empty;
    PairWithPosition pair = null, last = null;
    for (;;)
      {
	int ch = read();
	if (ch < 0)
	  break;
	if (ch == ' ' || ch == '\t')
	  continue;
	unread();
	if (ch == ')')
	  break;
	line = port.getLineNumber();
	int column = port.getColumnNumber();
	while (ch == '\r' || ch == '\n')
	  {
	    if (singleLine())
	      return obj;
	    ch = read();
	    skipIndentation(); // skipHorSpace.
	    column = port.getColumnNumber();
	    ch = peek();
	    if (column <= startColumn)
	      break;
	  }
	if (column <= startColumn && last != null)
	  break;
	Object next;
	if (column == lastColumn && last != null)
	  next = readCommand();
	else if (column < lastColumn && last != null)
	  {
	    PairWithPosition p = pair;
	    for (;;)
	      {
		Object n = p.cdr;
		if (n == LList.Empty)
		  break;
		PairWithPosition np = (PairWithPosition) n;
		int pColumn = np.getColumnNumber()-1;
		if (pColumn >= column)
		  {
		    if (pColumn > column)
		      error('e', "some tokens on previous line indented more than current line");
		    n = np.cdr;
		    if (n != LList.Empty)
		      {
			if (((PairWithPosition) n).getColumnNumber()-1==column)
			  {
			    p = (PairWithPosition) n;
			    continue;
			  }
			last = (PairWithPosition)
			  makePair(np, port.getLineNumber(), column);
			p.cdr = last;
		      }
		    break;
		  }
		p = np;
	      }
	    next = readCommand();
	  }
	else
	  next = readObject();
	if (next == Sequence.eofValue)
	  break;
	lastColumn = column;
	String filename = port.getName();
	PairWithPosition cur = PairWithPosition.make(next, LList.Empty,
						     filename, line+1, column+1);
	if (last == null)
	  {
	    pair = cur;
	    obj = cur;
	  }
	else if (last.car instanceof Keyword)
	  {
	    Object name = new QuoteExp(((Keyword) last.car).getName());
	    last.car
	      = new PairWithPosition(last, MakeAttribute.makeAttribute,
				     new PairWithPosition(last, name, cur));
	    continue;
	  }
	else
	  last.cdr = cur;
	last = cur;
      }
    if (! forceList)
      {
	if (obj == last)
	  obj = last.car;
	else if (last == null)
	  obj = QuoteExp.voidExp;
      }
    return obj;
  }

  public static Object readObject(InPort port)
      throws java.io.IOException, SyntaxException
  {
    return (new Q2Read(port)).readObject();
  }

  /** Record '[' location for error messages. */ 
  String expressionStartFile;
  int expressionStartLine;
  int expressionStartColumn;

  void saveExpressionStartPosition()
  {
    expressionStartFile = port.getName();
    expressionStartLine = port.getLineNumber();
    expressionStartColumn = port.getColumnNumber();
  }
}

class Q2ReaderParens extends ReaderDispatchMisc
{
  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    Q2Read reader = (Q2Read) in;
    char saveReadState = reader.pushNesting('(');
    try
      {
	Object result = reader.readCommand(true);

	LineBufferedReader port = reader.getPort();
	if (port.read() != ')')
	  reader.error("missing ')'");
	return result;
      }
    finally
      {
	reader.popNesting(saveReadState);
      }
  }

}
