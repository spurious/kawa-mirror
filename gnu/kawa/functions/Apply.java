package gnu.kawa.functions;
import gnu.lists.*;
import gnu.mapping.*;

/** Implement the standard Scheme function "apply".
 * This has been generalized so that the last (list argument)
 * can be any sequence, or any primitive array coercible to Object[]. */

public class Apply extends ProcedureN
{
  public static final Apply apply = new Apply();
  static { apply.setName("apply"); }

  private Object[] getArguments (Object[] args, int skip)
  {
    int count = args.length;
    if (count < skip + 1)
      throw new WrongArguments("apply",2,"(apply proc [args] args) [count:"+count+" skip:"+skip+"]");
    Object last = args[count-1];
    int last_count;
    if (last instanceof Object[])
      {
	Object[] last_arr = (Object[]) last;
	if (count == 2)
	  return last_arr;
	last_count = last_arr.length;
      }
    else if (last instanceof Sequence)
      last_count = ((Sequence)last).size();
    else
      last_count = -1;
    if (last_count < 0)
      throw new WrongType(this, count, last, "sequence or array");
    int numArgs = last_count + (count - skip - 1);
    Object[] proc_args = new Object[numArgs];
    int i;
    for (i = 0; i < count - skip - 1; i++)
      proc_args[i] = args[i+skip];
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
	      proc_args[i++] = last_seq.get(j);
	  }
      }
    return proc_args;
  }

  public static Object doApply(Procedure proc, Object[] args) throws Throwable
  {
    return proc.applyN(apply.getArguments(args, 0));
  }

  public Object applyN (Object[] args) throws Throwable
  {
    return ((Procedure) args[0]).applyN(getArguments(args, 1));
  }

  public void apply (CallContext ctx) throws Throwable
  {
    Object[] args = ctx.getArgs();
    ((Procedure) args[0]).checkN(getArguments(args, 1), ctx);
  }
}
