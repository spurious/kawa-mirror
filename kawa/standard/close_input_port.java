package kawa.standard;
import kawa.lang.*;
             
public class close_input_port extends Procedure1
{
  public close_input_port()
  {
    super("close-input-port");
  }

  public Object apply1 (Object arg1)
    throws GenericError
  {
    try {
       ((InPort)arg1).close();
    } catch (java.io.IOException e) {
      throw new GenericError(e.getMessage());
    }
    return Interpreter.voidObject;
  }
}
