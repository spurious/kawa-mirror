package gnu.kawa.brl;
import gnu.kawa.lispexpr.*;
import gnu.text.SyntaxException;
import gnu.mapping.*;
import gnu.text.SourceMessages;
import gnu.lists.UnescapedData;
import gnu.lists.Sequence;

/** A class to read Scheme forms (S-expressions). */

public class BRLRead extends LispReader
{
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
    char saveReadState = port.readState;
    try
      {
	for (;;)
	  {
	    int ch = port.read();
	    if (saveReadState == ']')
	      {
		if (ch == '[')
		  {
		    if (port.peek() == '[')
		      {
			port.read();
			tokenBufferAppend(ch);
			continue;
		      }
		    else
		      {
			saveReadState = '\n';
		      }
		  }
		else if (ch == '\n' && isInteractive())
		  {
		    port.unread();
		    tokenBufferAppend(ch);
		  }
		else if (ch >= 0)
		  {
		    tokenBufferAppend(ch);
		    continue;
		  }
		int length = tokenBufferLength - startPos;
		if (length > 0)
		  {
		    return new UnescapedData(new String(tokenBuffer,
							startPos, length));
		  }
		else if (ch < 0)
		  return Sequence.eofValue; // FIXME;
	      }
	    else
	      {
		if (ch < 0)
		  return Sequence.eofValue; // FIXME;
		if (ch == ']')
		  saveReadState = ']';
		else
		  {
		    Object value = readValues(ch);
		    if (value != Values.empty)
		      {
			if (value == gnu.expr.QuoteExp.voidExp)
			  value = Values.empty;
			return value;
		      }
		  }
	      }
	  }
      }
    finally
      {
	tokenBufferLength = startPos;
	((InPort) port).readState = saveReadState;
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


  public static ReadTable brlReadTable;
  static
  {
    brlReadTable = ReadTable.getInitial();
  }
}
