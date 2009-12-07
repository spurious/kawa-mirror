package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;

public class define_alias extends Syntax
{
  public static final define_alias define_alias = new define_alias();
  static { define_alias.setName("define-alias"); }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Object formCdr = st.getCdr();
    SyntaxForm formSyntax = null;
    while (formCdr instanceof SyntaxForm)
      {
	formSyntax = (SyntaxForm) formCdr;
	formCdr = formSyntax.getDatum();
      }
    if (formCdr instanceof Pair)
      {
        Pair p = (Pair) formCdr;
        SyntaxForm nameSyntax = formSyntax;
        Object name = p.getCar();
        while (name instanceof SyntaxForm)
          {
            nameSyntax = (SyntaxForm) name;
            name = nameSyntax.getDatum();
          }
        formCdr = p.getCdr();
        while (formCdr instanceof SyntaxForm)
          {
            formSyntax = (SyntaxForm) formCdr;
            formCdr = formSyntax.getDatum();
          }
        if ((name instanceof String || name instanceof Symbol)
            && formCdr instanceof Pair
            && (p = (Pair) formCdr).getCdr() == LList.Empty)
          {
            Declaration decl = tr.define(name, nameSyntax, defs);
            decl.setIndirectBinding(true);
            decl.setAlias(true);
            Expression arg = tr.rewrite_car(p, formSyntax);
            if (arg instanceof ReferenceExp)
              {
                ReferenceExp rarg = (ReferenceExp) arg;
                Declaration d = Declaration.followAliases(rarg.getBinding());
                Expression dval;
                if (d != null
                    && ((dval = d.getValue()) instanceof ClassExp
                        || dval instanceof ModuleExp))
                  {
                    decl.setIndirectBinding(false);
                    decl.setFlag(Declaration.IS_CONSTANT);
                  }
                else
                  rarg.setDontDereference(true);
              }
            else if (arg instanceof QuoteExp)
              {
                decl.setIndirectBinding(false);
                decl.setFlag(Declaration.IS_CONSTANT);
              }
            else
              {
                arg = location.rewrite(arg, tr);
                decl.setType(ClassType.make("gnu.mapping.Location"));
              }
            tr.mustCompileHere(); // For simplicity.
            tr.push(decl);
            SetExp sexp = new SetExp(decl, arg);
            tr.setLineOf(sexp);
            decl.noteValue(arg);
            sexp.setDefining (true);
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
