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

  public Future (Procedure0 action)
  {
    this.action = action;
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
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
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
	if (exception instanceof GenericError)
	  throw (GenericError)exception;
	if (exception instanceof UnboundSymbol)
	  throw (UnboundSymbol)exception;
	if (exception instanceof WrongType)
	  throw (WrongType)exception;
	if (exception instanceof WrongArguments)
	  throw (WrongArguments)exception;
	throw new GenericError (exception.toString());
      }
    return result;
  }
}
