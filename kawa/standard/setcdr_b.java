package kawa.standard;
import kawa.lang.*;

public class setcdr_b extends Procedure2
{
  public setcdr_b()
  {
    super("set-cdr!");
  }

  public Object apply2 (Object arg1, Object arg2) throws WrongType
  {
    if (arg1 instanceof Pair)
      {
	((Pair)arg1).cdr = arg2;
	return Interpreter.voidObject;
      }
    else
      throw new kawa.lang.WrongType(this.name,1,"pair");
  }
}
