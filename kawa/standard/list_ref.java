package kawa.standard;
import kawa.lang.*;

public class list_ref extends Procedure2
{
  public list_ref ()
  {
    super("list-ref");
  }

  public Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (! (arg2 instanceof java.lang.Integer))
      throw new WrongType(this.name,2,"integer");
    Object tail = list_tail.listTail (arg1,
				      ((java.lang.Integer)arg2).intValue());
    if (tail instanceof Pair)
      return ((Pair)tail).car;
    else
      throw new GenericError("List is too short.");
  }
}
