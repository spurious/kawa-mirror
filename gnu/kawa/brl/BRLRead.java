package gnu.kawa.brl;
import gnu.kawa.lispexpr.*;
import gnu.text.SyntaxException;
import gnu.mapping.*;
import gnu.text.SourceMessages;
import gnu.lists.*;

/** A class to read Scheme forms (S-expressions). */

public class BRLRead extends LispReader
{
  int nesting;

  /** True if in literal text (even if nested inside an escaped expression). */
  public boolean inLiteral ()
  {
    return ((InPort) port).readState == ']';
  }

  void init()
  {
    initialColonIsKeyword = false;
    set_string_start(']');
    ((InPort) port).readState = ']';
  }

  public BRLRead(InPort port)
  {
    super(port);
    init();
  }
  
  public BRLRead(InPort port, SourceMessages messages)
  {
    super(port, messages);
    init();
  }
  
  protected ReadTable getReadTable () { return brlReadTable; }

  public Object readObject ()
      throws java.io.IOException, SyntaxException
  {
    int startPos = tokenBufferLength;
    InPort port = (InPort) getPort();
    int saveNesting = nesting;
    try
      {
	for (;;)
	  {
	    int ch = port.read();
	    if (ch < 0)
	      {
		if (port.readState != ']' && ! isInteractive())
		  error('e', expressionStartFile,
			expressionStartLine + 1, expressionStartColumn,
			"an unmatched '[' was read");
		  return Sequence.eofValue; // FIXME;
	      }
	    if (port.readState == ']')
	      {
		port.unread();
		Object value = brlReader.read(this, ']', 1);
		if (ch == '[' && value == BRL.emptyForm)
		  continue;
		return value;
	      }
	    else
	      {
		if (ch == ']')
		  port.readState = ']';
		else
		  {
		    nesting++;
		    Object value = readValues(ch);
		    if (value != Values.empty)
		      {
			if (value == gnu.expr.QuoteExp.voidExp)
			  value = Values.empty;
			return value;
		      }
		    nesting = saveNesting;
		  }
	      }
	  }
      }
    finally
      {
	nesting = saveNesting;
	tokenBufferLength = startPos;
	//((InPort) port).readState = saveReadState;
      }
  }

  public static Object readObject(InPort port)
      throws java.io.IOException, SyntaxException
  {
    return (new BRLRead(port)).readObject();
  }

  public BRLReaderString brlReader =  new BRLReaderString();

  public void set_string_start(char c)
  {
    brlReadTable = ReadTable.getInitial();
    brlReadTable.setBracketMode(1);
    brlReadTable.set(c, brlReader);
  }

  boolean brlCompatible = false;

  public boolean isBrlCompatible() { return brlCompatible; }
  public void setBrlCompatible(boolean compat)
  {
    brlCompatible = compat;
    initialColonIsKeyword = compat;
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

  public static ReadTable brlReadTable;
  static
  {
    brlReadTable = ReadTable.getInitial();
  }
}
