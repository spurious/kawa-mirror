package kawa.standard;
import kawa.lang.*;

public class vector_p extends Procedure1
{
  public vector_p()
  {
    super("vector?");
  }

  public Object apply1 (Object arg1)
  {
    if (arg1 instanceof Vector)
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
