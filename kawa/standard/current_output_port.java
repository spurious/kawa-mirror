package kawa.standard;
import kawa.lang.*;
             
public class current_output_port extends Procedure0
{
  public current_output_port()
  {
    super("current-output-port");
  }

  public Object apply0 ()
  {
    return OutPort.outDefault ();
  }
}
