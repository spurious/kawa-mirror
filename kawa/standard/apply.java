package kawa.standard;
import kawa.lang.*;

/** Implement the standard Scheme function "apply".
 * This has been generalized so that the last (list argument)
 * can be any sequence, or any primitive array coercible to Object[]. */

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
    int last_count;
    if (last instanceof Object[])
      {
	Object[] last_arr = (Object[]) last;
	if (count == 2)
	  return proc.applyN (last_arr);
	last_count = last_arr.length;
      }
    else if (last instanceof Sequence)
      last_count = ((Sequence)last).length();
    else
      last_count = -1;
    if (last_count < 0)
      throw new WrongType(this.name(),count,"sequence");
    Object[] proc_args = new Object[last_count + (count - 2)];
    int i;
    for (i = 0; i < count - 2; i++)
      proc_args[i] = args[i+1];
    if (last instanceof Object[])
      {
	System.arraycopy((Object[]) last, 0,
			 proc_args, i, last_count);
      }
    else
      {
	while (last instanceof Pair)
	  {
	    Pair pair = (Pair) last;
	    proc_args[i++] = pair.car;
	    last = pair.cdr;
	    last_count--;
	  }
	if (last_count > 0)
	  {
	    Sequence last_seq = (Sequence) last;
	    for (int j = 0;  j < last_count; j++)
	      proc_args[i++] = last_seq.elementAt(j);
	  }
      }
    return proc.applyN (proc_args);
  }
}
