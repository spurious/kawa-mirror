package gnu.mapping;

public class Future extends Thread
{
  Object result;
  CallContext context;

  private Environment initEnvironment;
  InPort in;
  OutPort out;
  OutPort err;
  Throwable exception;

  Procedure action;

  public Future (Procedure action, Environment environment)
  {
    this(action, environment,
	 InPort.inDefault(), OutPort.outDefault(), OutPort.errDefault());
  }

  public Future (Procedure action, Environment environment,
		 InPort in, OutPort out, OutPort err)
  {
    this.action = action;
    this.initEnvironment = environment;
    ((SimpleEnvironment) environment).makeShared();
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
	context.curEnvironment = Environment.make(getName(), initEnvironment);
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
