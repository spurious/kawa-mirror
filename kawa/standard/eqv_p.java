package kawa.standard;
import kawa.lang.*;
import gnu.math.*;
import gnu.mapping.*;

/** Implement that standard Scheme function "eqv?". */

public class eqv_p extends Procedure2
{
  public boolean isEqv (Object arg1, Object arg2) 
  {
    if (arg1==arg2)
      return true;
    if (arg1 instanceof Char || arg1 instanceof Numeric)
      return arg1.equals (arg2);
    return false;
   }

  public Object apply2 (Object arg1, Object arg2)
  {
    return Interpreter.boolObject (isEqv (arg1, arg2));
   }
}
