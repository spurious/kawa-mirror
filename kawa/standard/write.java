package kawa.standard;
import kawa.lang.*;
import gnu.mapping.Procedure1or2;
import gnu.mapping.WrongType;
import gnu.mapping.WrongType;
import gnu.mapping.OutPort;

public class write extends Procedure1or2
{
  boolean readable;
  public write (boolean readable)
  {
    super (readable ? "write" : "display");
    this.readable = readable;
  }

  public final Object apply1 (Object arg1)
  {
    OutPort.outDefault().writeSchemeObject (arg1, readable);
    return Interpreter.voidObject;
  }

  public final Object apply2 (Object arg1,Object arg2)
  {
    if (! (arg2 instanceof OutPort))
      throw new WrongType (this.name (), 1, "output port");
    ((OutPort)arg2).writeSchemeObject (arg1, readable);
    return Interpreter.voidObject;
  }

}
