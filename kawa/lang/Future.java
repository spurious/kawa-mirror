package kawa.lang;

public class Future extends Thread
{
  Object result;
  Future parent;

  Environment environment;
  InPort in;
  OutPort out;
  OutPort err;
  Exception exception;

  Procedure0 action;

  public Future (Procedure0 action, Environment environment)
  {
    this.action = action;
    Thread parent_thread = Thread.currentThread();
    this.environment = environment;
  }

  public Future (Procedure0 action)
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
      
    environment = new Environment (parent_env);
  }

  public void run ()
  {
    try
      {
	result = action.apply0 ();
      }
    catch (Exception ex)
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
	throw new GenericError ("thread join [force] was interrupted");
      }
    if (exception != null)
      {
	if (exception instanceof RuntimeException)
	  throw (RuntimeException) exception;
	throw new GenericError (exception.toString());
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
