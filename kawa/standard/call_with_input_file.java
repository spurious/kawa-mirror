package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
             
public class call_with_input_file extends Procedure2
{
  public Object apply2 (Object string, Object proc)
  {
    String fname = string.toString();

    try {
      InPort port = InPort.openFile(fname);
      Object result = ((Procedure)proc).apply1 (port);
      port.close ();
      return result;
    } catch (java.io.FileNotFoundException e) {
       throw new RuntimeException ("file not found: " + fname);
    }
    catch (java.io.IOException e) {
      throw new WrappedException(e);
    }
  }
}
