package kawa.standard;
import kawa.lang.*;

public class write extends Procedure1or2
{
  boolean readable;
  public write (boolean readable)
  {
    super (readable ? "write" : "display");
    this.readable = readable;
  }

  public final Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    OutPort.outDefault().writeSchemeObject (arg1, readable);
    return Interpreter.voidObject;
  }

  public final Object apply2 (Object arg1,Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (! (arg2 instanceof OutPort))
      throw new WrongType (this.name (), 1, "output port");
    ((OutPort)arg2).writeSchemeObject (arg1, readable);
    return Interpreter.voidObject;
  }

}
