package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
             
public class call_with_output_file extends Procedure2
{
  public Object apply2 (Object string, Object proc)
  {
    String fname = string.toString();

    try
      {
	java.io.Writer os = new java.io.FileWriter(fname);
	OutPort port = new OutPort(os, fname);
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
