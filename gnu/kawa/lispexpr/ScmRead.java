package gnu.kawa.lispexpr;
import gnu.text.SyntaxException;
import gnu.mapping.*;
import gnu.text.SourceMessages;

/** A class to read Scheme forms (S-expressions). */

public class ScmRead extends LispReader
{
  public ScmRead(InPort port)
  {
    super(port);
    //initialColonIsKeyword = false;
  }
  
  public ScmRead(InPort port, SourceMessages messages)
  {
    super(port, messages);
    //initialColonIsKeyword = false;
  }
  
  protected ReadTable getReadTable () { return schemeReadTable; }

  public static ReadTable makeSchemeReadTable()
  {
    ReadTable tab = ReadTable.getInitial();
    ReaderDispatch dispatchTable = ReaderDispatch.getInitial();
    dispatchTable.set('\'', new ReaderQuote("syntax"));
    tab.set('#',  dispatchTable);
    return tab;
  }

  public static ReadTable schemeReadTable;
  static
  {
    schemeReadTable = makeSchemeReadTable();
  }

  public static Object readObject(InPort port)
      throws java.io.IOException, SyntaxException
  {
    return (new ScmRead(port)).readObject();
  }
}
