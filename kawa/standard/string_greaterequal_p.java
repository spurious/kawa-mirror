package kawa.standard;
import kawa.lang.*;

/** Implement the standard Scheme procedure "string>=?". */

public class string_greaterequal_p extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    if (arg1.toString().compareTo(arg2.toString()) >= 0)
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
