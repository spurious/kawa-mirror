package kawa.standard;
import kawa.lang.*;

public class symbol_p extends Procedure1
{
  public symbol_p()
  {
    super("symbol?");
  }

  public Object apply1 (Object arg1)
  {
    if (arg1 instanceof Symbol)
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
   }
}
