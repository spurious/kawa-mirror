package gnu.commonlisp.lang;
import gnu.kawa.lispexpr.*;
import gnu.mapping.*;
import gnu.text.*;

/** A class to read CommonLisp forms (S-expressions). */

public class CLispReader extends LispReader
{
  public CLispReader (InPort port)
  {
    super(port);
  }

  public CLispReader(InPort port, SourceMessages messages)
  {
    super(port, messages);
  }
  
  protected ReadTable getReadTable () { return clispReadTable; }

  protected Object makeSymbol (String name)
  {
    return CommonLisp.getSymbol(name.intern());
  }

  public static Object readObject(InPort port)
      throws java.io.IOException, SyntaxException
  {
    return (new CLispReader(port)).readObject();
  }

  public static ReadTable clispReadTable;
  static
  {
    clispReadTable = ReadTable.getInitial();
  }
}
