package kawa.standard;
import kawa.lang.*;
import gnu.mapping.Procedure2;
import gnu.mapping.WrongType;

public class setcdr_b extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    if (arg1 instanceof Pair)
      {
	((Pair)arg1).cdr = arg2;
	return Interpreter.voidObject;
      }
    else
      throw new WrongType(this.name (), 1, "pair");
  }
}
