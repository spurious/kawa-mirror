package kawa.standard;
import kawa.lang.*;

public class null_p extends Procedure1
{
  public null_p()
  {
    super("null?");
  }

  public final Object apply1 (Object arg1)
  {
    if (arg1 == List.Empty)
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
