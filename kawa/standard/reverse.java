package kawa.standard;
import gnu.mapping.Procedure1;
import gnu.mapping.WrongType;
import gnu.kawa.util.*;

public class reverse extends Procedure1
{
  public Object apply1 (Object arg)
  {
    Object result = LList.Empty;
    while (arg instanceof Pair)
      {
	Pair pair = (Pair) arg;
	result = new Pair (pair.car, result);
	arg = pair.cdr;
      }
    if (arg != LList.Empty)
      throw new WrongType(this.name (), 1, "list");
    return result;
  }
}
