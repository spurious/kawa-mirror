package gnu.q2.lang;
import gnu.kawa.lispexpr.*;
import gnu.text.SyntaxException;
import gnu.mapping.*;
import gnu.text.SourceMessages;
import gnu.lists.*;

/** A class to read Scheme forms (S-expressions). */

public class Q2Read extends LispReader
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
  
  protected ReadTable getReadTable () { return q2ReadTable; }

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

  public static ReadTable q2ReadTable;
  static
  {
    q2ReadTable = ReadTable.getInitial();
  }
}
