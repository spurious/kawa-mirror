package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

public class exit extends Procedure0or1
{
  public Object apply0 ()
  {
    System.exit (0);
    return Interpreter.undefinedObject;  // Never reached
  }

  public Object apply1 (Object arg1)
  {
    int status = IntNum.intValue (arg1);
    System.exit (status);
    return Interpreter.undefinedObject;  // Never reached
  }
}
