package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;

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
	      prim_throw.throw_it(key);
	  }
	else if (key instanceof String)
	  throw new NamedException((String) key, args);
      }
    throw (new GenericError("bad arguments to throw"));
  }
}

