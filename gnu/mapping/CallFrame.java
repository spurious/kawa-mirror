package gnu.mapping;

/** A procedure activation frame (when compiled with explicit stacks).
 * Not yet working/useful. */

public abstract class CallFrame extends MethodProc
implements Cloneable, CpsMethodContainer
{
  public Object[] args;
  public Procedure proc;
  protected Procedure caller;
  protected int saved_pc;

  protected int numArgs;

  public int numArgs() { return numArgs; }

  public void apply(CpsMethodProc proc, CallContext context)
  {
    context.pc = proc.selector;
    step(context);
  }

  public void apply (CallContext stack)
  {
    Object[] args = stack.values;
    Procedure.checkArgCount(this, args.length);
    try
      {
	CallFrame frame = (CallFrame) clone();
	frame.args = args;
	frame.caller = stack.proc;
	frame.saved_pc = stack.pc;
	frame.proc = this;
	stack.proc = frame;
	stack.pc = saved_pc;
      }
    catch (java.lang.CloneNotSupportedException ex)
      {
	throw new InternalError(ex.toString());
      }
  }

  public abstract void step (CallContext stack);
}
