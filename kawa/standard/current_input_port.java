package kawa.standard;
import kawa.lang.*;
             
public class current_input_port extends Procedure0
{
  public current_input_port()
  {
    super("current-input-port");
  }

  public Object apply0 ()
  {
    return InPort.inDefault ();
  }
}
