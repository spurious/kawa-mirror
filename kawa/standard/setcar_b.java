package kawa.standard;
import kawa.lang.*;

public class setcar_b extends Procedure2
{
  public setcar_b()
  {
    super("set-car!");
  }

  public Object apply2 (Object arg1, Object arg2) throws WrongType
  {
    if (arg1 instanceof Pair)
      {
	((Pair)arg1).car = arg2;
	return Interpreter.undefinedObject;
      }
    else
      throw new WrongType(this.name,1,"pair");
  }
}
