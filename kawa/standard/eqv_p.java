package kawa.standard;
import kawa.lang.*;

import kawa.lang.Procedure2;

public class eqv_p extends kawa.lang.Procedure2
{
  public kawa.standard.eqv_p()
  {
    super("eqv?");
  }

  public boolean eqv_p (Object arg1, Object arg2) 
  {
    if (arg1==arg2)
      return true;
    else if (arg1 instanceof Boolean && arg2 instanceof Boolean)
      return ((Boolean)arg1).booleanValue () == ((Boolean)arg2).booleanValue();
    else if (arg1 instanceof Double && arg2 instanceof Double)
      return (((Double)arg1).equals((Double)arg2));
    else if (arg1 instanceof Integer && arg2 instanceof Integer)
      return ((Integer)arg1).intValue () == ((Integer)arg2).intValue ();
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
