package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;

public class define_alias extends Syntax implements Printable
{
  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Pair p;
    Object name;
    if (! (st.cdr instanceof Pair)
        || (! ((name = (p = (Pair) st.cdr).car) instanceof String)
	    && ! (name instanceof Symbol))
	|| ! (p.cdr instanceof Pair)
	|| (p = (Pair) p.cdr).cdr != LList.Empty)
      {
	tr.error('e', "invalid syntax for define-alias");
	return false;
      }
    Declaration decl = defs.addDeclaration(name);
    decl.setIndirectBinding(true);
    decl.setAlias(true);
    Expression arg = tr.rewrite_car(p, false);
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

  public Expression rewrite (Object obj, Translator tr)
  {
    return tr.syntaxError ("define-alias is only allowed in a <body>");
  }
}
