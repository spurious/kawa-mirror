package gnu.mapping;

/** A procedure activation stack (when compiled with explicit stacks). */

public class CallStack implements Runnable
{
  public Procedure proc;

  /** The program location in the current procedure. */
  public int pc;

  /** Where incoming arguments are passed. */
  public Object[] args;

  /** Function results are left here. */
  public Object value;

  public void run()
  {
    for (;;)
      {
	Procedure proc = this.proc;
	if (proc == null)
	  break;
	//System.err.println("step "+proc);
	this.proc = null;
	proc.apply(this);
      }
  }
}
