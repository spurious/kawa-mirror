package kawa.standard;
import kawa.lang.*;

public class char_ready_p extends Procedure0or1 {
  public char_ready_p ()
  {
    super("char-ready?");
  }

  public final Object apply0 ()
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return apply1 (InPort.inDefault ());
  }

  public final Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (! (arg1 instanceof InPort))
      throw new WrongType (this.name(), 1, "input port");
    // FIXME should return #t if EOF was seen.
    try
      {
	if (((InPort) arg1).available () > 0)
	  return Interpreter.trueObject;
	else
	  return Interpreter.falseObject;
      }
    catch (java.io.IOException e)
      {
	throw new GenericError ("caught I/O exception: " + e);
      }
  }
}
