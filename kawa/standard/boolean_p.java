package kawa.standard;
import kawa.lang.*;

/** Implement the standard Scheme function "boolean?". */

public class boolean_p extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    if (arg1 instanceof Boolean)
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
