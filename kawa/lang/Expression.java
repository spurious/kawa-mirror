package kawa.lang;
import gnu.bytecode.Type;

/**
 * Abstract class for syntactic forms that evaluate to a value.
 * Scheme S-expressions get re-written to these before evaluation.
 * @author	Per Bothner
 */

public abstract class Expression implements Printable
{
  public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    throw new GenericError ("internal error - "
			    + getClass() + ".eval called");
  }

  abstract public void print (java.io.PrintWriter ps);

  /** If IGNORED is set in the flags passed to compile, the result is ignored.
   * Hence, do not leave any result on the stack. */
  public static final int IGNORED = 1;
       
  /** Set when compiling an expression that is executed last.
   * I.e. if this is a call, it is a tail-call. */
  public static final int LAST = 2;

  abstract public void compile (Compilation comp, int flags);

  /** Same as compile, but emit line number beforehard. */
  public final void compile_with_linenumber (Compilation comp, int flags)
  {
    int line = getLine ();
    if (line > 0)
      comp.method.compile_linenumber (line);
    compile (comp, flags);
  }

  public final void compile (Compilation comp, int flags, Type type)
  {
    compile (comp, flags);
    type.emitCoerceFromObject(comp.getCode());
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
