package kawa.standard;
import kawa.lang.*;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure1;

public class symbol2string extends kawa.lang.Procedure1
{
  public kawa.standard.symbol2string()
  {
    super("symbol->string");
  }

  public Object apply1 (Object arg1)
       throws WrongType
  {
    if (arg1 instanceof symbol)
      return new java.lang.StringBuffer(((symbol)arg1).toString());
    else
      throw new WrongType(this.name,1,"symbol");
  }
}
