package gnu.mapping;
import gnu.math.*;

/** A procedure activation stack (when compiled with explicit stacks). */

public class CallContext implements Runnable
{
  public Procedure proc;

  /* CPS: ??
  CallFrame frame;
  */

  /** The program location in the current procedure. */
  public int pc;

  /** Function results are left here. */
  public Object value; // FIXME

  /** Used for passing parameters and result. */
  public Object value1;
  public Object value2;
  public Object value3;
  public Object value4;
  public Object[] values;
  public int ivalue1;
  public int ivalue2;

  /** Number of actual arguments. */
  public int count;
  
  /** Encoding of where the arguments are.
   * Each argument uses 4 bits.
   * Arguments beyond 8 are implicitly ARG_IN_VALUES_ARRAY.
   */
  int where;
  final static int ARG_IN_VALUES_ARRAY = 0;
  final static int ARG_IN_VALUE1 = 1;
  final static int ARG_IN_VALUE2 = 2;
  final static int ARG_IN_VALUE3 = 3;
  final static int ARG_IN_VALUE4 = 4;
  final static int ARG_IN_IVALUE1 = 5;
  final static int ARG_IN_IVALUE2 = 6;

  public Object getArgAsObject(int i)
  {
    if (i < 8)
      {
        switch ((this.where >> (4 * i)) & 15)
          {
          case ARG_IN_VALUE1:  return value1;
          case ARG_IN_VALUE2:  return value2;
          case ARG_IN_VALUE3:  return value3;
          case ARG_IN_VALUE4:  return value4;
          case ARG_IN_IVALUE1:  return IntNum.make(ivalue1);
          case ARG_IN_IVALUE2:  return IntNum.make(ivalue2);
          }
      }
    return values[i];
  }

  public void setArgs()
  {
    count = 0;
    where = 0;
  }

  public void setArgs(Object arg1)
  {
    value1 = arg1;
    count = 1;
    where = ARG_IN_VALUE1;
  }

  public void setArgs(Object arg1, Object arg2)
  {
    value1 = arg1;
    value2 = arg2;
    count = 2;
    where = ARG_IN_VALUE1|(ARG_IN_VALUE2<<4);
  }
  public void setArgs(Object arg1, Object arg2, Object arg3)
  {
    value1 = arg1;
    value2 = arg2;
    value3 = arg3;
    count = 3;
    where = ARG_IN_VALUE1|(ARG_IN_VALUE2<<4)|(ARG_IN_VALUE3<<8);
  }

  public void setArgs(Object arg1, Object arg2, Object arg3, Object arg4)
  {
    value1 = arg1;
    value2 = arg2;
    value3 = arg3;
    value4 = arg4;
    count = 4;
    where = (ARG_IN_VALUE1|(ARG_IN_VALUE2<<4)
      |(ARG_IN_VALUE3<<8|ARG_IN_VALUE4<<12));
  }

  public void setArgsN(Object[] args)
  {
    values = args;
    count = args.length;
    where = 0;
  }

  public Object[] getArgs()
  {
    if (where == 0)
      return values;
    else
      {
	int i = count;
	Object[] args = new Object[i];
	while (--i >= 0)
	  args[i] = getArgAsObject(i);
	return args;
      }
  }

  public void run()
  {
    for (;;)
      {
	/** Cps
	CallFrame frame = this.frame;
	if (frame == null)
	  break;
	frame.step(this);
	*/
	Procedure proc = this.proc;
	if (proc == null)
	  break;
	//System.err.println("step "+proc);
	this.proc = null;
	proc.apply(this);
      }
  }
}
