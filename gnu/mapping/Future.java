package gnu.mapping;

public class Future extends Thread
                    // FUTURE: implements java.util.concurrent.Future
{
  public RunnableClosure closure;

  public Future (Procedure action, CallContext parentContext)
  {
    closure = new RunnableClosure (action, parentContext);
  }

  public Future (Procedure action,
		 InPort in, OutPort out, OutPort err)
  {
    closure = new RunnableClosure (action, in, out, err);
  }

  public Future (Procedure action)
  {
    closure = new RunnableClosure(action);
  }

  public static Future make (Procedure action, Environment penvironment,
                             InPort in, OutPort out, OutPort err)
  {
    Environment saveEnv = Environment.setSaveCurrent(penvironment);
    try
      {
        return new Future(action, in, out, err);
      }
    finally
      {
        Environment.restoreCurrent(saveEnv);
      }
  }

  /** Get the CallContext we use for this Thread. */
  public final CallContext getCallContext() {
    return closure.getCallContext();
  }

  public void run() {
    closure.run();
  }

  public Object waitForResult ()  throws Throwable
  {
    try
      {
	join ();
      }
    catch (InterruptedException ex)
      {
	throw new RuntimeException ("thread join [force] was interrupted");
      }
    Throwable ex = closure.exception;
    if (ex != null)
      throw ex;
    return closure.result;
  }

  public String toString() {
    StringBuffer buf = new StringBuffer();
    buf.append ("#<future ");
    buf.append(getName());
    buf.append(">");
    return buf.toString();
  }
}
