package kawa.lang;
import codegen.Type;

/**
 * Abstract class for syntactic forms that evaluate to a value.
 * Scheme S-expressions get re-written to these before evaluation.
 * @author	Per Bothner
 */

public abstract class Expression implements Printable
{
  abstract public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError;

  abstract public void print (java.io.PrintStream ps);

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
    if (type == Type.char_type)
      { // We handle char specially, because Kawa does not use standard
	// java.lang.Character type.
	Char.initMakeMethods();
	comp.method.compile_checkcast (Char.scmCharType);
	comp.method.compile_invoke_virtual (Char.charValueMethod);
      }
    else
      type.compileCoerceFromObject(comp.method);
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

  public final int getLine ()
  {
    return position >> 12;
  }

  public final int getColumn ()
  {
    return position & ((1 << 12) - 1);
  }

}
