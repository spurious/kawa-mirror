package kawa.standard;
import kawa.lang.*;

public class list_ref extends Procedure2
{
  protected Procedure2 list_tail;
  public list_ref (Procedure2 proc)
  {
    super("list-ref");
    list_tail = proc;
  }

  public Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Object tail = list_tail.apply2 (arg1, arg2);
    if (tail instanceof Pair)
      return ((Pair)tail).car;
    else
      throw new GenericError("List is too short.");
  }
}
