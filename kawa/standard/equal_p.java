package kawa.standard;
import kawa.lang.*;
import gnu.expr.Interpreter;

/** Implement the standard Scheme procedure "equal?". */

public class equal_p extends gnu.mapping.Procedure2
{
  Interpreter interpreter;

  public equal_p(Interpreter interpreter)
  {
    this.interpreter = interpreter;
  }

  public static boolean apply (Object arg1, Object arg2)
  {
    return arg1 == arg2 || (arg1 != null && arg1.equals (arg2));
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    return interpreter.booleanObject(apply(arg1, arg2));
  }

}
