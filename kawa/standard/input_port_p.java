package kawa.standard;
import kawa.lang.*;
             
public class input_port_p extends Procedure1
{
  public input_port_p()
  {
    super("input-port?");
  }

  public Object apply1 (Object arg1)
  {
    if (arg1 instanceof InPort)
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
