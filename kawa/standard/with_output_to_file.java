package kawa.standard;
import gnu.mapping.*;
             
public class with_output_to_file extends Procedure2
{
  public with_output_to_file ()
  {
    super("with-output-to-file");
  }

  public Object apply2 (Object string, Object proc) throws Throwable
  {
    String fname = string.toString();

    java.io.Writer is = new java.io.FileWriter(fname);
    OutPort port = new OutPort(is, fname);
    Object result;
    OutPort save_port = OutPort.outDefault ();
    try
      {
	OutPort.setOutDefault (port);
	result = ((Procedure)proc).apply0 ();
      }
    finally
      {
	OutPort.setOutDefault (save_port);
	port.close ();
      }
    return result;
  }
}
