package kawa.standard;
import kawa.lang.*;

public class list_tail extends Procedure2
{
   public list_tail()
  {
    super("list-tail");
  }

  static public Object listTail (Object list, int count)
       throws WrongArguments, WrongType, GenericError
  {
    while (--count >= 0)
      {
	if (! (list instanceof Pair))
	  throw new GenericError("List is too short.");
	Pair pair = (Pair) list;
	list = pair.cdr;
      }
    return list;
  }

  public Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError
  {
    if (arg1 instanceof Pair)
      {
	if (arg2 instanceof java.lang.Integer)
	  {
            Pair list = (Pair)arg1;
            int count = ((java.lang.Integer)arg2).intValue();
	    return listTail (list, count);
	  }
	else
	  throw new WrongType(this.name,2,"integer");
      }
    else
      throw new WrongType(this.name,1,"list");
  }
}
