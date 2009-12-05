package kawa.lang;
import gnu.expr.*;
import java.io.*;

/** A scope created when expanding a SyntaxTemplate.
 * This is used to ensure proper "hygiene". */

public class TemplateScope extends LetExp implements Externalizable
{
  /** The module instance containing the defining macro.
   * If we're expanding a macro imported from some external module,
   * the macro's template(s) may expand to references to declarations in
   * that external module. If the module is non-static, we may need a
   * context instance to access those declarations; we inherit the context
   * instance from the declaration bound to the imported macro.
   * This is used to setContextDecl() of such references. */
  Declaration macroContext;

  private Syntax syntax; // Only used for debugging

  public TemplateScope ()
  {
    super(null);
  }

  public TemplateScope (ScopeExp outer)
  {
    super(null);
    this.outer = outer;
  }

  public static TemplateScope make ()
  {
    return make((Translator) Compilation.getCurrent());
  }

  public static TemplateScope make (Translator tr)
  {
    TemplateScope templateScope = new TemplateScope();
    Syntax curSyntax = tr.getCurrentSyntax();
    if (curSyntax instanceof Macro)
      {
        templateScope.outer = ((Macro) curSyntax).getCapturedScope();
        templateScope.macroContext = tr.macroContext;
      }
    templateScope.syntax = curSyntax;
    return templateScope;
  }

  public String toString() { return super.toString()+"(for "+syntax+")"; }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(outer);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    outer = (ScopeExp) in.readObject();
  }
}
