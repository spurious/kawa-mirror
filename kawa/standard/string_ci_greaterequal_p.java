package kawa.standard;
import kawa.lang.*;
import gnu.mapping.Procedure2;

/** Implement the standard Scheme procedure "string-ci>=?". */

public class string_ci_greaterequal_p extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    if (arg1.toString().toLowerCase().compareTo(
             arg2.toString().toLowerCase()
          ) >= 0)
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }

}
