package kawa.lang;

/**
 * Abstract class for "syntax" objects.
 * Builtins and macros are instances of this class.
 * @author	Per Bothner
 */

abstract public class Syntax extends Named implements Printable
{
  public Syntax () { super (); }

  public Syntax (Symbol name) { super (name); }

  /**
   * Re-write an expression that is an "application" of this Syntax object.
   * @param obj the arguments to this "application" (i.e. the cdr of
   * the macro/builtin invokation)
   * @param tr the Translator that provides context
   * @return the re-written expression
   */
  abstract public Expression rewrite (Object obj, Translator tr);

  public void print(java.io.PrintStream ps)
  {
    ps.print("#<syntax ");
    Symbol name = this.name ();
    if (name == null)
      ps.print ("<unnamed>");
    else
      ps.print(name);
    ps.print ('>');
  }
}
