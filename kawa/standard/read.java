package kawa.standard;
import kawa.lang.*;

public class read extends Procedure0or1 {
  public read()
  {
    super("read");
  }

  public final Object apply0 ()
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return apply1 (InPort.inDefault());
  }

  public final Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (! (arg1 instanceof InPort))
      throw new WrongType (this.name(), 0, "input port");
    try
      {
	return ((InPort)arg1).readSchemeObject ();
      }
    catch (ReadError e)
      {
	throw new GenericError ("syntax error in read: " + e.toString ());
      }
    catch (java.io.IOException e)
      {
	throw new GenericError ("I/O exception in read: " + e.toString ());
      }
  }
}
