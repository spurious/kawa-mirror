package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
             
public class call_with_output_file extends Procedure2
{
  public Object apply2 (Object string, Object proc) throws Throwable
  {
    String fname = string.toString();

    java.io.Writer os = new java.io.FileWriter(fname);
    OutPort port = new OutPort(os, fname);
    Object result = ((Procedure)proc).apply1 (port);
    port.close ();
    return result;
  }
}
