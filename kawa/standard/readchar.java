package kawa.standard;
import kawa.lang.*;

public class readchar extends Procedure0or1
{
  boolean peeking;
  public readchar(boolean peeking)
  {
    super(peeking ? "peek-char" : "read-char");
    this.peeking = peeking;
  }

  final Object read_char (InPort port)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    try
      {
	int ch = port.readChar ();
	if (ch < 0)
	  return Interpreter.eofObject;
	if (peeking)
	  port.unreadChar ();
	return Char.make (ch);
      }
    catch (java.io.IOException e)
      {
	throw new GenericError ("IO Exception caught");
      }
  }

  public final Object apply0 ()
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return read_char (InPort.inDefault());
  }

  public final Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (! (arg1 instanceof InPort))
      throw new WrongType (this.name (), 1, "input port");
    return read_char ((InPort) arg1);
  }
}
