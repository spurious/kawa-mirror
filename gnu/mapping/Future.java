package gnu.mapping;

public class Future extends Thread
{
  Object result;
  Future parent;

  public FluidBinding fluidBindings;

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

  public void setFluids (FluidBinding new_fluids)
  {
    FluidBinding old_fluids = fluidBindings;
    FluidBinding fluid = new_fluids;
    for ( ; fluid != old_fluids;  fluid = fluid.previous)
      {
	Binding binding = fluid.binding;
	Constraint constraint = binding.constraint;
	if (constraint instanceof FluidConstraint)
	  ((FluidConstraint) constraint).referenceCount++;
	else
	  binding.constraint = new FluidConstraint(constraint);
      }
    fluidBindings = new_fluids;
  }

  public void resetFluids (FluidBinding old_fluids)
  {
    FluidBinding new_fluids = fluidBindings;
    FluidBinding fluid = new_fluids;
    for ( ; fluid != old_fluids;  fluid = fluid.previous)
      {
	Binding binding = fluid.binding;
	FluidConstraint constraint = (FluidConstraint) binding.constraint;
	if (constraint.referenceCount-- <= 0)
	  binding.constraint = constraint.savedConstraint;
      }
    fluidBindings = old_fluids;
  }

  public String toString() {
    StringBuffer buf = new StringBuffer();
    buf.append ("#<future ");
    buf.append(getName());
    buf.append(">");
    return buf.toString();
  }


  static Future defaultContext = null;

  static synchronized void getDefaultContext ()
  {
    if (defaultContext == null)
      defaultContext = new Future(null);
  }

  /** Get chain of FluidBindings for the current thread (if a Future).
   * Should fix to work with other threads. */

  public static FluidBinding getFluids()
  {
    Thread thread = Thread.currentThread();
    /*
    for (;;)
      {
	if (thread instanceof Future)
	  return ((Future)thread).fluidBindings;
	FluidBinding fl = ....get(thread);
	if (fl != null)
	  return fl;
	thread = thread.getParent();
      }
    */
    if (thread instanceof Future)
      return ((Future)thread).fluidBindings;
    if (defaultContext == null)
      getDefaultContext();
    return defaultContext.fluidBindings;
  }

  public static Future getContext ()
  {
    Thread thread = Thread.currentThread();
    if (thread instanceof Future)
      return (Future)thread;
    if (defaultContext == null)
      getDefaultContext();
    return defaultContext;
  }
}
