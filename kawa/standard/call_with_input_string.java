package kawa.standard;
import kawa.lang.*;
             
public class call_with_input_string extends Procedure2
{
  public static InPort open_input_string (String string)
  {
    // FIXME:  This is rather clumsy.  We first write the String to a
    // byte array, and then read it back.  This is to handle
    // non-Ascii characters.  A more generalized InPort would be better.
    java.io.ByteArrayOutputStream bout = new java.io.ByteArrayOutputStream ();
    OutPort tmp = new OutPort (bout);
    tmp.print (string);
    java.io.ByteArrayInputStream is =
      new java.io.ByteArrayInputStream (bout.toByteArray ());
    return new InPort(is, "<string>");
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
