package kawa.standard;
import gnu.mapping.*;
import java.io.PrintWriter;

/** A TracedProcedure is a Procedure wrapper that writes trace output. */

public class TracedProcedure extends ProcedureN
{
  public Procedure proc;
  boolean enabled;

  public TracedProcedure (Procedure proc, boolean enable)
  {
    this.proc = proc;
    this.enabled = enable;
    String name = proc.getName();
    if (name != null)
      setName(name);
  }

  static void put(Object value, PrintWriter out)
  {
    try
      {
	if (! ObjectFormat.format(value, out, 50, true))
	  out.print("...");
      }
    catch (java.io.IOException ex)
      {
	out.print("<caught ");
	out.print(ex);
	out.print('>');
      }
  }

  public Object applyN(Object[] args)
  {
    if (enabled)
      {
	PrintWriter out = OutPort.errDefault();
	String name = getName();
	if (name == null)
	  name = "??";
	out.print("call to ");
	out.print(name);
	int len = args.length;
	out.print(" (");
	for (int i = 0;  i < len;  i++)
	  {
	    if (i > 0)
	      out.print(' ');
	    put(args[i], out);
	  }
	out.println(")");
	Object result = proc.applyN(args);
	out.print("return from ");
	out.print(name);
	out.print(" => ");
	put(result, out);
	out.println();
	return result;
      }
    return proc.applyN(args);
  }

  public static Procedure doTrace(Procedure proc, boolean enable)
  {
    if (proc instanceof TracedProcedure)
      {
	((TracedProcedure) proc).enabled = enable;
	return proc;
      }
    else
      return new TracedProcedure(proc, enable);
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<procedure ");
    String n = name ();
    if (n == null)
      ps.print ("<unnamed>");
    else
      ps.print (n);
    ps.print(enabled? ", traced>" : ">");
  }
}
