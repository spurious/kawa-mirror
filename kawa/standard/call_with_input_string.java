package kawa.standard;
import kawa.lang.*;
import java.io.CharArrayReader;
             
public class call_with_input_string extends Procedure2
{
  public static InPort open_input_string (String string)
  {
    return new InPort(new CharArrayReader(string.toCharArray()), "<string>");
  }

  public Object apply2 (Object string, Object proc)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    try {
      InPort port = open_input_string (string.toString ());
      Object result = ((Procedure)proc).apply1 (port);
      port.close ();
      return result;
    }
    catch (java.io.IOException e) {
      throw new GenericError ("caught I/O exception: " + e);
    }
  }
}
