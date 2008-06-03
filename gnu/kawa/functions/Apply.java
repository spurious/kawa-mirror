package gnu.kawa.functions;
import gnu.lists.*;
import gnu.mapping.*;

/** Implement the standard Scheme function "apply".
 * This has been generalized so that the last (list argument)
 * can be any sequence, or any primitive array coercible to Object[]. */

public class Apply extends ProcedureN
{
  ApplyToArgs applyToArgs;

  public Apply(String name, ApplyToArgs applyToArgs)
  {
    super(name);
    this.applyToArgs = applyToArgs;
  }

  public static Object[] getArguments (Object[] args, int skip, Procedure proc)
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
      throw new WrongType(proc, count, last, "sequence or array");
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
	    proc_args[i++] = pair.getCar();
	    last = pair.getCdr();
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

  public Object applyN (Object[] args) throws Throwable
  {
    return applyToArgs.applyN(getArguments(args, 0, this));
  }

  public void apply (CallContext ctx) throws Throwable
  {
    Object[] args = ctx.getArgs();
    applyToArgs.checkN(getArguments(args, 0, this), ctx);
  }
}
