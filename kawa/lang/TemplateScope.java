package kawa.lang;
import gnu.bytecode.ClassType;
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

  /** See Translator#currentMacroMark. */
  Object macroMark;

  private Syntax syntax; // Only used for debugging

  public TemplateScope ()
  {
  }

  public TemplateScope (ScopeExp outer)
  {
    this.outer = outer;
  }

  public static TemplateScope make ()
  {
    return make((Translator) Compilation.getCurrent());
  }

    public static TemplateScope make (Translator tr) {
        TemplateScope templateScope = new TemplateScope();
        if (tr != null) {
            templateScope.macroMark = tr.currentMacroMark;
            Syntax curSyntax = tr.getCurrentSyntax();
            if (curSyntax instanceof Macro) {
                templateScope.outer = ((Macro) curSyntax).getCapturedScope();
                templateScope.macroContext = tr.macroContext;
            }
            templateScope.syntax = curSyntax;
        }
        return templateScope;
    }

    public static TemplateScope make(ModuleExp module, String mname) {
        TemplateScope templateScope = new TemplateScope();
        templateScope.outer = module;
        return templateScope;
    }

    void init(Macro macro) {
        outer = macro.getCapturedScope();
        macroContext = outer.lookup(macro.getName());
        syntax = macro;
        macroMark = macro;
    }

    public static TemplateScope make(String moduleClassName) {
        TemplateScope templateScope = new TemplateScope();
        templateScope.setOuter(moduleClassName);
        return templateScope;
    }

    void setOuter(String moduleClassName) {
        outer = ModuleInfo.find(ClassType.make(moduleClassName)).getModuleExp();
    }

    public String toString() { return super.toString()+"(for "+syntax+")"; }

  public void writeExternal(ObjectOutput out) throws IOException
  {
      String moduleClassName = null;
      if (outer instanceof ModuleExp) {
          ClassType moduleClass = ((ModuleExp) outer).getClassType();
          if (moduleClass != null)
              moduleClassName = moduleClass.getName();
      }
      out.writeObject(moduleClassName);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    in.readObject(); // ignore, for now
  }
}
