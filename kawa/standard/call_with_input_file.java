package kawa.standard;
import kawa.lang.*;
             
public class call_with_input_file extends Procedure2
{
  public call_with_input_file()
  {
    super("call-with-input-file");
  }

  public Object apply2 (Object string, Object proc)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    String fname = string.toString();

    try {
      InPort port = InPort.openFile(fname);
      Object result = ((Procedure)proc).apply1 (port);
      port.close ();
      return result;
    } catch (java.io.FileNotFoundException e) {
       throw new GenericError ("file not found: " + fname);
    }
    catch (java.io.IOException e) {
      throw new GenericError ("caught I/O exception: " + e);
    }
  }
}
