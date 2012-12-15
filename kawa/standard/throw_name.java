package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.kawa.reflect.Throw;

public class throw_name extends ProcedureN
{
  public static final throw_name throwName = new throw_name();

  public Object applyN (Object[] args)
    throws Throwable
  {
    int len = args.length;
    if (len > 0)
      {
	Object key = args[0];
	if (key instanceof Throwable)
	  {
	    if (args.length == 1)
	      Throw.doThrow(key);
	  }
	else if (key instanceof Symbol)
	  throw new NamedException((Symbol) key, args);
      }
    throw (new GenericError("bad arguments to throw"));
  }
}

