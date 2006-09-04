package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;

/**
 * Implement the Scheme standard function "call-with-current-continuation".
 * This is a restricted version, that only works for escape-like applications.
 * @author Per Bothner
 */

public class callcc extends MethodProc
{
  public static final callcc callcc = new callcc();

  public int numArgs() { return 0x1001; }

  public int match1 (Object proc, CallContext ctx)
  {
    if (! (proc instanceof Procedure))
      return NO_MATCH_BAD_TYPE;
    return super.match1(proc, ctx);
  }

  public void apply (CallContext ctx)  throws Throwable
  {
    Procedure proc = (Procedure) ctx.value1;
    Continuation cont = new Continuation(ctx);
    proc.check1(cont, ctx);
    proc = ctx.proc;
    ctx.proc = null;
    try
      {
	proc.apply(ctx);
	ctx.runUntilDone();
      }
    catch (CalledContinuation ex)
      {
	if (ex.continuation != cont)
	  throw ex;
	Object[] values = ex.values;
	int nvalues = values.length;
	for (int i = 0;  i < nvalues;  i++)
	  ctx.consumer.writeObject(ex.values[i]);
      }
    finally
      {
	cont.invoked = true;
      }
  }

  /*
  public void apply (CallContext stack)
  {
    kawa.lang.Continuation cont = new Continuation ();
    cont.frame = stack.proc;
    cont.pc = stack.pc;
    stack.value = cont;
  }
  */
}

/*
class Continuation extends MethodProc
{
  Procedure frame;
  int pc;

  public void apply (CallContext stack)
  {
    Object result = Values.make(stack.args);
    stack.pc = pc;
    stack.proc = frame;
    stack.result = result;
  }
}
*/
