package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
             
public class call_with_input_file extends Procedure2
{
  public Object apply2 (Object string, Object proc) throws Throwable
  {
    String fname = string.toString();

    InPort port = InPort.openFile(fname);
    Object result = ((Procedure)proc).apply1 (port);
    port.close ();
    return result;
  }
}
