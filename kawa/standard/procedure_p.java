package kawa.standard;
import kawa.lang.*;

/** Implement the standard Scheme function "procedure?". */

public class procedure_p extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    if (arg1 instanceof Procedure)
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
