package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.kawa.reflect.*;
import gnu.bytecode.*;

public class define_member_alias extends Syntax
{
  public static final define_member_alias define_member_alias
    = new define_member_alias();
  static { define_member_alias.setName("define-member-alias"); }


  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Pair p;
    if (! (st.cdr instanceof Pair)
        || (tr.currentScope() instanceof ModuleExp)
        || ! ((p = (Pair) st.cdr).car instanceof String))
      return super.scanForDefinitions(st, forms, defs, tr);
    Object name = p.car;
    Declaration decl = defs.addDeclaration((String) name,
                                           Compilation.typeSymbol);
    decl.setIndirectBinding(true);
    st = Translator.makePair(st, this, Translator.makePair(p, decl, p.cdr));
    forms.addElement(st);
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.cdr;
    Pair p1;
    if (! (obj instanceof Pair)
        || ! ((p1 = (Pair) obj).car instanceof String
              || p1.car instanceof Declaration))
      return tr.syntaxError("missing name in " + getName());
    if (p1.cdr instanceof Pair)
      {
        String name;
        Declaration decl;
        if (p1.car instanceof Declaration)
          {
            decl = (Declaration) p1.car;
            name = decl.getName();
          }
        else
          {
            name = (String) p1.car;
            decl = null;
          }
        Pair p2 = (Pair) p1.cdr;
        Expression fname = null;
        Expression arg = tr.rewrite(p2.car);
        if (p2.cdr == LList.Empty)
          fname = new QuoteExp(gnu.expr.Compilation.mangleName(name));
        else if (p2.cdr instanceof Pair)
          {
            Pair p3 = (Pair) p2.cdr;
            if (p3.cdr == LList.Empty)
              fname = tr.rewrite(p3.car);
          }
        if (fname != null)
          {
            ClassType t
              = ClassType.make("gnu.kawa.reflect.ClassMemberConstraint");
            Expression[] args = new Expression[3]; 
            args[0] = new QuoteExp(name);
            args[1] = arg;
            args[2] = fname;
            return Invoke.makeInvokeStatic(t, "define", args);
          }
      }
    return tr.syntaxError("invalid syntax for " + getName());
  }
}
