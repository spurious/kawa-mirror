package kawa.standard;
import kawa.lang.*;

/** Implement the standard Scheme function "symbol->string". */

public class symbol2string extends Procedure1
{
  public Object apply1 (Object arg1)
       throws WrongType
  {
    if (arg1 instanceof Symbol)
      return new FString (((Symbol)arg1).toString());
    else
      throw new WrongType (this.name(), 1, "symbol");
  }
}
