package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;

public class define_namespace extends Syntax
{
  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    if (! (st.cdr instanceof Pair))
      return super.scanForDefinitions(st, forms, defs, tr);
    Pair p = (Pair) st.cdr;
    Object name = p.car;
    Pair namePair = p;
    String sym = null;
    if (name instanceof String)
      {
	sym = ("xmlns:" + name).intern();
	Declaration decl = defs.getDefine(sym, 'w', tr);
	tr.pushBinding(sym, decl);
	decl.setFlag(Declaration.IS_CONSTANT);
	p = tr.makePair(p, decl, p.cdr);
	st = tr.makePair(st, this, p);
        if (defs instanceof ModuleExp)
	  decl.setCanRead(true);
	Translator.setLine(decl, namePair);
      }
    forms.addElement (st);
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.cdr;
    String name = null;
    Expression value = null;
    Declaration decl = null;

    if (obj instanceof Pair)
      {
	Pair p1 = (Pair) obj;
	if (p1.car instanceof String && p1.cdr instanceof Pair)
	  {
	    Pair p2 = (Pair) p1.cdr;
	    if (p2.cdr == LList.Empty)
	      {
		name = (String) p1.car;
		value = tr.rewrite (p2.car);
	      }
	  }
	else if (p1.car instanceof Declaration && p1.cdr instanceof Pair)
	  {
	    decl = (Declaration) p1.car;
	    Pair p2 = (Pair) p1.cdr;
	    Pair p3;
	    if ("::" == p2.car && p2.cdr instanceof Pair
		&& (p3 = (Pair) p2.cdr).cdr instanceof Pair)
	      {
		decl.setType(kawa.standard.prim_method.exp2Type(p3.car, tr));
		decl.setFlag(Declaration.TYPE_SPECIFIED);
		p2 = (Pair) p3.cdr;
	      }
	    if (p2.cdr == LList.Empty)
	      {
                name = decl.getName();
		value = tr.rewrite (p2.car);
	      }
	  }
      }
    if (name == null)
      return tr.syntaxError ("invalid syntax for "+getName());
    SetExp sexp = new SetExp (name, value);
    sexp.setDefining (true);
    if (decl != null)
      {
	sexp.binding = decl;
	decl.noteValue(value);
      }
    return sexp;
  }
}
