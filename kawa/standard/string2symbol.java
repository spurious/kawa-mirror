package kawa.standard;
import kawa.lang.*;
import gnu.mapping.Procedure1;
import gnu.mapping.WrongType;

/**
 * Implement the Scheme standard function "string->symbol".
 */

public class string2symbol extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    // Note string->symbol does *not* fold case, according to R4RS.
    if (arg1 instanceof FString)
      return ((FString)arg1).toString().intern();
    else
      throw new WrongType(this.name(),1,"string");
  }
}
