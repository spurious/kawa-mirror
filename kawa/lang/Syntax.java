package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * Abstract class for "syntax" objects.
 * Builtins and macros are instances of this class.
 * @author	Per Bothner
 */

abstract public class Syntax extends Named implements Printable
{
  public Syntax () { super (); }

  public Syntax (String name) { super (name); }

  /**
   * Re-write an expression that is an "application" of this Syntax object.
   * @param obj the arguments to this "application" (i.e. the cdr of
   * the macro/builtin invokation)
   * @param tr the Translator that provides context
   * @return the re-written expression
   */
  public Expression rewrite (Object obj, Translator tr)
  {
    throw new InternalError("rewrite method not defined");
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return rewrite(form.cdr, tr);
  }

  /*
  public Expression apply(SyntaxForm form)
  {
    return rewriteForm((Pair) form.form, form.tr);
  }

  public Object apply1(Object arg)
  {
    SyntaxForm form;
    try
      {
        form = (SyntaxForm) arg;
      }
    catch (ClassCastException ex)
      {
        throw WrongType.make(ex, this, 0);
      }
    return apply(form);
  }
  */

  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<syntax ");
    String name = this.getName();
    if (name == null)
      ps.print ("<unnamed>");
    else
      ps.print(name);
    ps.print ('>');
  }
}
