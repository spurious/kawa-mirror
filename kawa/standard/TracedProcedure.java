package kawa.standard;
import gnu.mapping.*;
import java.io.PrintWriter;
import gnu.math.IntNum;
import gnu.kawa.functions.ObjectFormat;

/** A TracedProcedure is a Procedure wrapper that writes trace output. */

public class TracedProcedure extends ProcedureN
{
  public Procedure proc;
  boolean enabled;

  static Symbol indentation
  = Symbol.makeUninterned(gnu.math.IntNum.zero(), "indentation");
  static int indentationStep = 2;

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

  static void indent(int i, PrintWriter out)
  {
    while (--i >= 0)
      out.print(' ');
  }

  public Object applyN(Object[] args) throws Throwable
  {
    if (enabled)
      {
        int curIndent = ((IntNum) indentation.getValue()).intValue();
	PrintWriter out = OutPort.errDefault();
	String name = getName();
	if (name == null)
	  name = "??";

        // Print the call arguments (indented).
        indent(curIndent, out);
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

        // Now do the actual call, but with the indentation incremented.
        CallContext context = CallContext.getInstance();
        FluidBinding oldBindings = context.fluidBindings;
        IntNum newIndentation = IntNum.make(curIndent+indentationStep);
        FluidBinding newBindings
          = new FluidBinding(oldBindings, newIndentation, indentation);
	Object result;
        try
          {
            context.setFluids(newBindings);
            result = proc.applyN(args);
          }
        catch (RuntimeException e)
          {
            indent(curIndent, out);
            out.println("procedure " + name + " throws exception " + e);
            throw e;
          }
        finally
          {
            context.resetFluids(oldBindings);
          }

        // Print the result (indented).
        indent(curIndent, out);
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
    String n = getName();
    if (n == null)
      ps.print ("<unnamed>");
    else
      ps.print (n);
    ps.print(enabled? ", traced>" : ">");
  }
}
