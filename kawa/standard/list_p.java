package kawa.standard;
import kawa.lang.*;

public class list_p extends Procedure1
{
  public kawa.standard.list_p()
  {
    super("list?");
  }

  public Object apply1 (Object arg1)
  {
    if (List.list_length (arg1) >= 0)
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
