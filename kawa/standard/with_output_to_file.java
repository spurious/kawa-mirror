package kawa.standard;
import kawa.lang.*;
             
public class with_output_to_file extends Procedure2
{
  public with_output_to_file ()
  {
    super("with_output-to-file");
  }

  public Object apply2 (Object string, Object proc)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    String fname = string.toString();

    try {
      java.io.OutputStream is = new java.io.FileOutputStream(fname);
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
    } catch (java.io.FileNotFoundException e) {
       throw new GenericError ("file not found: " + fname);
    }
    catch (java.io.IOException e) {
      throw new GenericError ("caught I/O exception: " + e);
    }
  }
}
