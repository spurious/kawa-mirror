package gnu.mapping;

public class Future extends Thread
{
  Object result;
  Future parent;
  CallContext context;

  Environment environment;
  InPort in;
  OutPort out;
  OutPort err;
  Throwable exception;

  Procedure action;

  public Future (Procedure action, Environment environment)
  {
    this.action = action;
    Thread parent_thread = Thread.currentThread();
    this.environment = environment;
  }

  public Future (Procedure action, Environment environment,
		 InPort in, OutPort out, OutPort err)
  {
    this.action = action;
    Thread parent_thread = Thread.currentThread();
    this.environment = environment;
    this.in = in;
    this.out = out;
    this.err = err;
  }

  public Future (Procedure action)
  {
    this.action = action;
    in = InPort.inDefault();
    out = OutPort.outDefault();
    err = OutPort.errDefault();
    Thread parent_thread = Thread.currentThread();
    Environment parent_env;
    if (parent_thread instanceof Future)
      {
	parent = (Future)parent_thread;
	parent_env = parent.environment;
      }
    else
      parent_env = Environment.user();
      
    environment = parent_env;
  }

  /** Get the CallContext we use for this Thread. */
  public final CallContext getCallContext() { return context; }

  public void run ()
  {
    try
      {
	result = action.apply0 ();
      }
    catch (Throwable ex)
      {
	exception = ex;
      }
  }

  public Object waitForResult ()
  {
    try
      {
	join ();
      }
    catch (InterruptedException ex)
      {
	throw new RuntimeException ("thread join [force] was interrupted");
      }
    if (exception != null)
      {
	if (exception instanceof RuntimeException)
	  throw (RuntimeException) exception;
	throw new RuntimeException (exception.toString());
      }
    return result;
  }

  /**
   * @deprecated
   */
  public final void setFluids (FluidBinding new_fluids)
  {
    context.setFluids(new_fluids);
  }

  /**
   * @deprecated
   */
  public final void resetFluids (FluidBinding old_fluids)
  {
    context.resetFluids(old_fluids);
  }

  public String toString() {
    StringBuffer buf = new StringBuffer();
    buf.append ("#<future ");
    buf.append(getName());
    buf.append(">");
    return buf.toString();
  }

  /** Get chain of FluidBindings for the current thread (if a Future).
   * Should fix to work with other threads. */

  public static FluidBinding getFluids()
  {
    return CallContext.getInstance().fluidBindings;
  }
}
