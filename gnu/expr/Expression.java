package gnu.expr;
import gnu.bytecode.Type;
import gnu.mapping.*;

/**
 * Abstract class for syntactic forms that evaluate to a value.
 * Scheme S-expressions get re-written to these before evaluation.
 * @author	Per Bothner
 */

public abstract class Expression implements Printable
{
  public Object eval (Environment env)
  {
    throw new RuntimeException ("internal error - "
			        + getClass() + ".eval called");
  }

  abstract public void print (java.io.PrintWriter ps);

  public abstract void compile (Compilation comp, Target target);

  /** Same as compile, but emit line number beforehard. */
  public final void compileWithPosition(Compilation comp, Target target)
  {
    int line = getLine ();
    if (line > 0)
      comp.method.compile_linenumber (line);
    compile(comp, target);
  }

  public final void compile (Compilation comp, Type type)
  {
    compile (comp, new StackTarget(type));
  }

  String filename;
  int position;

  public final void setFile (String filename)
  {
    this.filename = filename;
  }

  public final void setLine (int lineno, int colno)
  {
    position = (lineno << 12) + colno;
  }

  public final void setLine (int lineno)
  {
    setLine (lineno, 0);
  }

  public final String getFile ()
  {
    return filename;
  }

  /** Get the line number of (the start of) this Expression.
    * The "first" line is line 1. */
  public final int getLine ()
  {
    return position >> 12;
  }

  public final int getColumn ()
  {
    return position & ((1 << 12) - 1);
  }

  /** Return the Type used to represent the values of this Expression. */
  public Type getType()
  {
    return Type.pointer_type;
  }

}
