package kawa.lang;

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

  String filename;
  int position;

  final void setFile (String filename)
  {
    this.filename = filename;
  }

  final void setLine (int lineno, int colno)
  {
    position = (lineno << 12) + colno;
  }

  final void setLine (int lineno)
  {
    setLine (lineno, 0);
  }

  final String getFile ()
  {
    return filename;
  }

  final int getLine ()
  {
    return position >> 12;
  }

  final int getColumn ()
  {
    return position & ((1 << 12) - 1);
  }

}
