package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

public class eqv_p extends Procedure2
{
  public kawa.standard.eqv_p()
  {
    super("eqv?");
  }

  public boolean eqv_p (Object arg1, Object arg2) 
  {
    if (arg1==arg2)
      return true;
    if (arg1 instanceof Boolean || arg1 instanceof Char
	|| arg1 instanceof IntNum || arg1 instanceof DFloNum)
      return arg1.equals (arg2);
    return false;
   }

  public Object apply2 (Object arg1, Object arg2)
  {
    if (eqv_p (arg1, arg2))
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
   }
}
