package kawa.standard;
import kawa.lang.*;

import kawa.lang.Procedure2;

public class equal_p extends kawa.lang.Procedure2
{
  public kawa.standard.equal_p()
  {
    super("equal?");
  }

  // This does not handle list or vector equality!!  FIXME
  public final static boolean equal_p (Object arg1, Object arg2)
  {
    if (arg1==arg2)
      return true;
    else if (arg1 instanceof Boolean && arg2 instanceof Boolean)
      return ((Boolean)arg1).booleanValue () == ((Boolean)arg2).booleanValue();
    else if (arg1 instanceof Double && arg2 instanceof Double)
      return (((Double)arg1).equals((Double)arg2));
    else if (arg1 instanceof Integer && arg2 instanceof Integer)
      return ((Integer)arg1).intValue () == ((Integer)arg2).intValue ();
    else if (arg1 instanceof Character && arg2 instanceof Character)
      return ((Character)arg1).charValue() == ((Character)arg2).charValue ();
    return false;
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    if (equal_p (arg1, arg2))
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
   }

}
