package kawa.standard;
import kawa.lang.*;

/** Implement the standard Scheme function "apply". */

public class apply extends ProcedureN
{
  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    int count = args.length;
    if (count < 2)
      throw new WrongArguments(this.name(),2,"(apply proc [args] args)");
    if (! (args[0] instanceof Procedure))
      throw new WrongType(this.name(),1,"procedure");
    Procedure proc = (Procedure) args[0];
    Object last = args[count-1];
    int last_count = List.length (last);
    Object[] proc_args = new Object[last_count + (count - 2)];
    int i;
    for (i = 0; i < count - 2; i++)
      proc_args[i] = args[i+1];
    while (last instanceof Pair)
      {
	Pair pair = (Pair) last;
	proc_args[i++] = pair.car;
	last = pair.cdr;
      }
    if (last != List.Empty)
      throw new WrongType(this.name(),count-1,"list");
    return proc.applyN (proc_args);
  }
}
