package gnu.mapping;

public class Future extends Thread
{
  Object result;
  CallContext context;
  Environment environment;

  // These are only used to when we need to override the parents' in/out/err
  // in the child.  This is not needed for normal Future objects, but (say)
  // when starting a repl in a new window.  In that case we could do te
  // in/out/err override in the 'action'.  FIXME.
  private InPort in;
  private OutPort out;
  private OutPort err;
  Throwable exception;

  Procedure action;

  public Future (Procedure action, Environment penvironment)
  {
    this.action = action;
    if (penvironment instanceof SimpleEnvironment)
      ((SimpleEnvironment) penvironment).makeShared();
    SimpleEnvironment env = Environment.make(getName(), penvironment); 
    env.makeShared();
    env.flags &= ~Environment.DIRECT_INHERITED_ON_SET;
    this.environment = env;
  }

  public Future (Procedure action, Environment penvironment,
		 InPort in, OutPort out, OutPort err)
  {
    this(action, penvironment);
    this.in = in;
    this.out = out;
    this.err = err;
  }

  public Future (Procedure action)
  {
    this(action, Environment.getCurrent());
  }

  /** Get the CallContext we use for this Thread. */
  public final CallContext getCallContext() { return context; }

  public void run ()
  {
    try
      {
	context = CallContext.getInstance();
	context.curEnvironment = environment;
	if (in != null)
	  InPort.setInDefault(in);
	if (out != null)
	  OutPort.setOutDefault(out);
	if (err != null)
	  OutPort.setErrDefault(err);
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

  public String toString() {
    StringBuffer buf = new StringBuffer();
    buf.append ("#<future ");
    buf.append(getName());
    buf.append(">");
    return buf.toString();
  }
}
