package kawa.standard;
import kawa.lang.*;

public class newline extends Procedure0or1
{
  public newline()
  {
    super("newline");
  }

  public final Object apply0 ()
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    outPort.outDefault().println ();
    return Interpreter.voidObject;
  }

  public final Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (! (arg1 instanceof OutPort))
      throw new WrongType (this.name, 1, "output port");
    ((OutPort) arg1).println ();
    return Interpreter.voidObject;
  }
}
