package kawa.standard;
import kawa.lang.*;
import gnu.mapping.Procedure1;
import gnu.mapping.WrongType;

public class reverse extends Procedure1
{
  public Object apply1 (Object arg)
  {
    Object result = List.Empty;
    while (arg instanceof Pair)
      {
	Pair pair = (Pair) arg;
	result = new Pair (pair.car, result);
	arg = pair.cdr;
      }
    if (arg != List.Empty)
      throw new WrongType(this.name (), 1, "list");
    return result;
  }
}
