package kawa.standard;
import kawa.lang.*;

public class eof_object_p extends Procedure1
{
  public eof_object_p ()
  {
    super ("eof-object?");
  }

  public Object apply1 (Object arg1)
  {
    if (arg1 == Interpreter.eofObject)
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
