package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.mapping.Symbol;

public class module_name extends Syntax
{
  public static final module_name module_name = new module_name();
  static { module_name.setName("module-name"); }

  public void scanForm (Pair form, ScopeExp defs, Translator tr)
  {
    Object form_cdr = form.cdr;
    SyntaxForm nameSyntax = null;
    while (form_cdr instanceof SyntaxForm)
      {
	nameSyntax = (SyntaxForm) form_cdr;
	form_cdr = nameSyntax.form;
      }
    Object arg = form_cdr instanceof Pair ? ((Pair) form_cdr).car : null;
    while (arg instanceof SyntaxForm)
      {
	nameSyntax = (SyntaxForm) arg;
	arg = nameSyntax.form;
      }
    String name = null;
    Pair p;
    String err = null;
    Declaration decl = null;
    if (arg instanceof Pair && (p = (Pair) arg).car == "quote")
      {
	arg = p.cdr;
	if (! (arg instanceof Pair)
	    || (p = (Pair) arg).cdr != LList.Empty
	    || ! (p.car instanceof String))
	  err = "invalid quoted symbol for 'module-name'";
        else
          name = (String) p.car;
      }
    else if (arg instanceof FString)
      name = arg.toString();
    else if (arg instanceof String || arg instanceof Symbol)
      {
	name = arg.toString();
	int len = name.length();
	if (len > 2
	    && name.charAt(0) == '<'
	    && name.charAt(len-1) == '>')
	  {
	    name = name.substring(1, len-1);
            decl = tr.define(arg, nameSyntax, defs);
	  }
	else
	  err = "not implemented: plain name in module-name";
      }
    else
      err = "un-implemented expression in module-name";
    if (err != null)
      tr.formStack.add(tr.syntaxError(err));
    else
      {
        int index = name.lastIndexOf('.');
        String className = name;
        if (index >= 0)
          tr.classPrefix = name.substring(0, index+1);
        else
          {
            name = tr.classPrefix + name;
            className = tr.classPrefix + Compilation.mangleName(name);
          }
        ModuleExp module = tr.getModule();
        if (tr.mainClass == null)
          tr.mainClass = new gnu.bytecode.ClassType(className);
        else
          {
            String oldName = tr.mainClass.getName();
            if (oldName == null)
              tr.mainClass.setName(className);
            else if (! oldName.equals(className))
              tr.syntaxError("duplicate module-name: old name: "+oldName);
          }
        module.setType(tr.mainClass);
        module.setName(name);

        if (decl != null)
          {
            decl.noteValue(module);
            decl.setFlag(Declaration.IS_CONSTANT|Declaration.PRIVATE_SPECIFIED
                         |Declaration.STATIC_SPECIFIED);
            decl.setPrivate(true);
            decl.setType(Compilation.typeClassType);
          }
        tr.mustCompileHere();
      }
  }
}
