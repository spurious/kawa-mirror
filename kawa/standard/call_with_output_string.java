package kawa.standard;
import kawa.lang.*;
             
public class call_with_output_string extends Procedure1
{
  public Object apply1 (Object proc)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    try
      {
	java.io.ByteArrayOutputStream bout
	  = new java.io.ByteArrayOutputStream ();
	OutPort port = new OutPort(bout, "<string>");
	((Procedure)proc).apply1 (port);
	port.close ();
	byte[] bresult = bout.toByteArray ();

	// FIXME:  We convert the multi-byte string (a byte[])
	// to a Unicode String by converting it to an InputPort.
	// Not terribly efficient or elegant ...
	java.io.ByteArrayInputStream bis =
	  new java.io.ByteArrayInputStream (bresult);
	InPort is = new InPort (bis, "<string>");
	StringBuffer buffer = new StringBuffer ();
	for (;;)
	  {
	    int c = is.readChar ();
	    if (c < 0)
	      break;
	    buffer.append ((char)c);
	  }
	return new FString(buffer);
      }
    catch (java.io.IOException e)
      {
	throw new GenericError ("caught I/O exception: " + e);
      }
  }
}
