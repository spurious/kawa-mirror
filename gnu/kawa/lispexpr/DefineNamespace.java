package gnu.kawa.lispexpr;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;

public class DefineNamespace extends Syntax
{
  public static final DefineNamespace define_namespace
    = new DefineNamespace();
  static { define_namespace.setName("define-namespace"); }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Pair p1, p2;
    if (! (st.cdr instanceof Pair)
        || ! ((p1 = (Pair) st.cdr).car instanceof String)
	|| ! (p1.cdr instanceof Pair)
	|| (p2 = (Pair) p1.cdr).cdr != LList.Empty)
      {
	tr.error('e', "invalid syntax for define-alias");
	return false;
      }
    String name = (Language.NAMESPACE_PREFIX + p1.car).intern();
    Declaration decl = defs.getDefine(name, 'w', tr);
    tr.push(decl);
    decl.setFlag(Declaration.IS_CONSTANT);
    if (defs instanceof ModuleExp)
      decl.setCanRead(true);
    Translator.setLine(decl, p1);
    Expression value = tr.rewrite_car (p2, false);
    SetExp sexp = new SetExp(decl, value);
    sexp.setDefining (true);
    decl.noteValue(value);
    forms.addElement (sexp);
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return tr.syntaxError ("define-namespace is only allowed in a <body>");
  }
}
