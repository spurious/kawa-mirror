package kawa.standard;
import kawa.lang.*;

public class list_tail extends Procedure2
{
   public list_tail()
  {
    super("list-tail");
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
            while (count!=0 && list.cdr instanceof Pair)
	      {
		count--;
		list = (Pair)list.cdr;
	      }
            if (count!=0)
	      throw new GenericError("List is too short.");
            return list;
	  }
	else
	  throw new WrongType(this.name,2,"integer");
      }
    else
      throw new WrongType(this.name,1,"list");
  }
}
