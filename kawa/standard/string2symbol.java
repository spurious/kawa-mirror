package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "string->symbol".
 */

public class string2symbol extends Procedure1
{
  public string2symbol()
  {
    super("string->symbol");
  }

  public Object apply1 (Object arg1)
       throws WrongType
  {
    // Note string->symbol does *not* fold case, according to R4RS.
    if (arg1 instanceof StringBuffer)
      return symbol.intern(((StringBuffer)arg1).toString());
    else
      throw new WrongType(this.name,1,"string");
  }
}
