package kawa.standard;
import kawa.lang.*;

public class exit extends Procedure0or1
{
  public Object apply0 ()
  {
    System.exit (0);
    return Interpreter.undefinedObject;  // Never reached
  }

  public Object apply1 (Object arg1)
  {
    int status = ((Number)arg1).intValue ();
    System.exit (status);
    return Interpreter.undefinedObject;  // Never reached
  }
}
