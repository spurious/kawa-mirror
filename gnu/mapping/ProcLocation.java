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

  public Object get () { return proc.applyN(args); }

  public void set (Object value)
  {
    Object[] xargs = new Object[args.length + 1];
    xargs[0] = value;
    System.arraycopy(args, 0, xargs, 1, args.length);
    proc.setN(xargs);
  }
}
