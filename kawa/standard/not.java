package kawa.standard;
import kawa.lang.*;

/** Implement the standard Scheme procedure "not". */

public class not extends Procedure1
{
  public Object apply1 (Object arg1)
   {
     return Interpreter.boolObject (arg1 == Scheme.falseObject);
   }
}
