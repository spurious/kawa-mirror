package kawa.standard;
import kawa.lang.*;
             
public class output_port_p extends Procedure1
{
  public output_port_p()
  {
    super("output-port?");
  }

  public Object apply1 (Object arg1)
  {
    if (arg1 instanceof OutPort) {
      return Interpreter.trueObject;
    } else {
      return Interpreter.falseObject;
    }
  }
}
