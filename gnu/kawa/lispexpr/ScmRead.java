package gnu.kawa.lispexpr;
import gnu.math.*;
import java.io.*;
import gnu.text.Char;
import gnu.text.SyntaxException;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;
import gnu.lists.*;
import java.util.Vector;

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
  
  protected ReadTable getReadTable () { return scmReadTable; }

  public static Object readObject(InPort port)
      throws java.io.IOException, SyntaxException
  {
    return (new ScmRead(port)).readObject();
  }

  public static ReadTable scmReadTable;
  static
  {
    scmReadTable = ReadTable.getInitial();
  }
}
