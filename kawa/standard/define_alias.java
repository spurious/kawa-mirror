package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;

public class define_alias extends Syntax implements Printable
{
  public static final define_alias define_alias = new define_alias();
  static { define_alias.setName("define-alias"); }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Object formCdr = st.cdr;
    SyntaxForm formSyntax = null;
    while (formCdr instanceof SyntaxForm)
      {
	formSyntax = (SyntaxForm) formCdr;
	formCdr = formSyntax.form;
      }
    if (formCdr instanceof Pair)
      {
        Pair p = (Pair) formCdr;
        SyntaxForm nameSyntax = formSyntax;
        Object name = p.car;
        while (name instanceof SyntaxForm)
          {
            nameSyntax = (SyntaxForm) name;
            name = nameSyntax.form;
          }
        formCdr = p.cdr;
        while (formCdr instanceof SyntaxForm)
          {
            formSyntax = (SyntaxForm) formCdr;
            formCdr = formSyntax.form;
          }
        if ((name instanceof String || name instanceof Symbol)
            && formCdr instanceof Pair
            && (p = (Pair) formCdr).cdr == LList.Empty)
          {
            Declaration decl = tr.define(name, nameSyntax, defs);
            decl.setIndirectBinding(true);
            decl.setAlias(true);
            Expression arg = tr.rewrite_car(p, formSyntax);
            if (arg instanceof ReferenceExp)
              ((ReferenceExp) arg).setDontDereference(true);
            else
              arg = location.rewrite(arg, tr);
            tr.mustCompileHere(); // For simplicity.
            tr.push(decl);
            SetExp sexp = new SetExp(decl, arg);
            tr.setLineOf(sexp);
            decl.noteValue(arg);
            sexp.setDefining (true);
            if (! (arg instanceof ReferenceExp))
              decl.setType(ClassType.make("gnu.mapping.Location"));
            forms.addElement(sexp);
            return true;
          }
      }
    tr.error('e', "invalid syntax for define-alias");
    return false;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    return tr.syntaxError ("define-alias is only allowed in a <body>");
  }
}
