package kawa.standard;
import kawa.lang.*;
             
public class close_output_port extends Procedure1
{
  public close_output_port()
  {
    super("close-output-port");
  }

  public Object apply1 (Object arg1)
    throws GenericError
  {
    ((OutPort)arg1).close();
    return Interpreter.voidObject;
  }
}
