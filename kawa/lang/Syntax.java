package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.util.*;

/**
 * Abstract class for "syntax" objects.
 * Builtins and macros are instances of this class.
 * @author	Per Bothner
 */

abstract public class Syntax extends Declaration implements Printable, Named
{
  public Syntax ()
  {
    setSimple(false);
    // Kludge to prevent BindingInitializer doing setIndirectBinding.
    setFlag(Declaration.TYPE_SPECIFIED);
    // Kludge to force BindingInitializer to set field FINAL.
    setFlag(Declaration.IS_CONSTANT);
  }

  public Syntax (String name)
  {
    this();
    setName(name);
  }

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

  /** Check if a statement is a definition, for initial pass.
   * @param st the statement to check
   * @param form where to append the (possibly-modified) statement
   * @param defs where to add Declarations for found definitions
   * @param tr the compilation state
   * @return true on success
   */
  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                    ScopeExp defs, Translator tr)
  {
    forms.addElement(st);
    return true;
  }

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
