package kawa.standard;
import kawa.lang.*;
             
public class with_input_from_file extends Procedure2
{
  public with_input_from_file ()
  {
    super("with_input_from_file");
  }

  public Object apply2 (Object string, Object proc)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    String fname = string.toString();

    try {
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
    } catch (java.io.FileNotFoundException e) {
       throw new GenericError ("file not found: " + fname);
    }
    catch (java.io.IOException e)
      {
	throw new GenericError ("caught I/O exception: " + e);
      }
  }
}
