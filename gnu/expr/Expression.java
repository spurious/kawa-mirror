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
      {
        comp.method.compile_linenumber (line);
        compileNotePosition(comp, target);
      }
    else
      compile(comp, target);
  }

  /** Compile, but take note of line number. */
  public final void compileNotePosition(Compilation comp, Target target)
  {
    String saveFilename = comp.filename;
    int savePosition = comp.position;
    try
      {
        comp.filename = filename;
        comp.position = position;
        compile(comp, target);
      }
    finally
      {
        comp.filename = saveFilename;
        comp.position = savePosition;
      }
  }

  public final void compile (Compilation comp, Type type)
  {
    // Should we use Target.pushValue instead?  FIXME.
    compile (comp, StackTarget.getInstance(type));
  }

  abstract Object walk (ExpWalker walker);

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
