package kawa.standard;
import kawa.lang.Procedure2;

/** Implement the standard Scheme procedure "string=?". */

public class string_equal_p extends kawa.lang.Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    if (arg1.toString().equals(arg2.toString()))
      return kawa.lang.Interpreter.trueObject;
    else
      return kawa.lang.Interpreter.falseObject;
  }
}
