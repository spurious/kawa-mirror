// Copyright (c) 1998  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/* A Location whose current value is given by a Procedure call. */

public class ProcLocation extends Location
{
  Procedure proc;
  Object[] args;

  public ProcLocation (Procedure proc, Object[] args)
  {
    this.proc = proc;
    this.args = args;
  }

  public Object get (Object defaultValue)
  {
    try
      {
	return proc.applyN(args);
      }
    catch (RuntimeException ex)
      {
	throw ex;
      }
    catch (Error ex)
      {
	throw ex;
      }
    catch (Throwable ex)
      {
	throw new WrappedException(ex);
      }
  }

  public void set (Object value)
  {
    int len = args.length;
    Object[] xargs = new Object[len + 1];
    xargs[len] = value;
    System.arraycopy(args, 0, xargs, 0, len);
    try
      {
	proc.setN(xargs);
      }
    catch (RuntimeException ex)
      {
	throw ex;
      }
    catch (Error ex)
      {
	throw ex;
      }
    catch (Throwable ex)
      {
	throw new WrappedException(ex);
      }
  }

  public boolean isBound ()
  {
    return true;
  }
}

