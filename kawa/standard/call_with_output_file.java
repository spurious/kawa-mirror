package kawa.standard;
import kawa.lang.*;
             
public class call_with_output_file extends Procedure2
{
  public call_with_output_file()
  {
    super("call-with-output-file");
  }

  public Object apply2 (Object string, Object proc)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    String fname = string.toString();

    try
      {
	java.io.OutputStream is = new java.io.FileOutputStream(fname);
	OutPort port = new OutPort(is, fname);
	Object result = ((Procedure)proc).apply1 (port);
	port.close ();
	return result;
      }
    catch (java.io.IOException e)
      {
	throw new GenericError ("caught I/O exception: " + e);
      }
  }
}
