// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

public class WrongType extends WrappedException
{
  //-- number of the argument, 0-origin
  // ARG_UNKNOWN mean unknown argument number
  // ARG_VARNAME means not a call, procname is a variable name.
  // ARG_DESCRIPTION means not a call, procname describes the target.
  public int number;

  public static final int ARG_UNKNOWN = -1;
  public static final int ARG_VARNAME = -2;
  public static final int ARG_DESCRIPTION = -3;

  //-- type of the argument
  public String typeExpected;
  //-- Procedure name that threw the exception
  public String procname;
  public Procedure proc;

  public WrongType(String name, int n, String u)
  {
    super(null, null);
    procname = name;
    number = n - 1;
    typeExpected = u;
  }

  public WrongType(Procedure proc, int n, ClassCastException ex)
  {
    super(ex);
    this.proc = proc;
    this.procname = proc.getName();
    this.number = n;
  }

  public WrongType(String procname, int n, ClassCastException ex)
  {
    super(ex);
    this.procname = procname;
    this.number = n;
  }

  /** This interface is designed for a compact call sequence. */
  public static WrongType make(ClassCastException ex, Procedure proc, int n)
  {
    return new WrongType(proc, n, ex);
  }

  /** This interface is designed for a compact call sequence. */
  public static WrongType make(ClassCastException ex, String procname, int n)
  {
    return new WrongType(procname, n, ex);
  }

  public String getMessage()
  {
    StringBuffer sbuf = new StringBuffer(100);
    if (number == ARG_VARNAME)
      {
        sbuf.append("Value for variable `");
        sbuf.append(procname);
        sbuf.append("' has wrong type");
      }
    else if (number == ARG_DESCRIPTION)
      {
        sbuf.append(procname);
        sbuf.append(" has wrong type");
      }
    else
      {
        sbuf.append("Argument ");
        if (number >= 0)
          {
            sbuf.append('#');
            sbuf.append(number);
          }
        sbuf.append(" to `");
        sbuf.append(procname);
        sbuf.append("' has wrong type");
      }
    Throwable ex = getCause();
    if (ex != null)
      {
        sbuf.append(" (");
        sbuf.append(ex.getMessage());
        sbuf.append(')');
      }
    return sbuf.toString();
  }
}
