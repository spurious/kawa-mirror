package kawa.standard;
import kawa.lang.*;

public class pair_p extends Procedure1
{
  public pair_p()
  {
    super("pair?");
  }

  public Object apply1 (Object arg1)
  {
    if (arg1 instanceof Pair)
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
