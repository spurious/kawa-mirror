package kawa.standard;
import kawa.lang.*;

import kawa.lang.Procedure2;

public class equal_p extends kawa.lang.Procedure2
{
  public kawa.standard.equal_p()
  {
    super("equal?");
  }

  public final static boolean equal_p (Object arg1, Object arg2)
  {
    return arg1 == arg2 || (arg1 != null && arg1.equals (arg2));
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    if (equal_p (arg1, arg2))
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
   }

}
