package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;

/** Implement the standard Scheme function "list?". */

public class list_p extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    if (List.list_length (arg1) >= 0)
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
