package kawa.standard;
import gnu.mapping.*;
             
public class with_input_from_file extends Procedure2
{
  public with_input_from_file ()
  {
    super("with_input_from_file");
  }

  public Object apply2 (Object string, Object proc) throws Throwable
  {
    String fname = string.toString();

    InPort port = InPort.openFile(fname);
    Object result;
    InPort save_port = InPort.inDefault ();
    try
      {
	InPort.setInDefault (port);
	result = ((Procedure)proc).apply0 ();
      }
    finally
      {
	InPort.setInDefault (save_port);
	port.close ();
      }
    return result;
  }
}
