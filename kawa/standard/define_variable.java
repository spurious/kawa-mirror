package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;

/** "define-variable" is like define, but ignored if variable already bound. */

public class define_variable extends Syntax implements Printable
{
  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    if (! (st.cdr instanceof Pair))
      return super.scanForDefinitions(st, forms, defs, tr);
    Pair p = (Pair) st.cdr;
    if (p.car instanceof String)
      {
	String sym = (String) p.car;
	Declaration decl = defs.lookup(sym);
	if (decl == null)
	  {
	    decl = new Declaration(sym);
	    defs.addDeclaration(decl);
	  }
	else
	  tr.error('w', "duplicate declaration for `"+sym+"'");
	p = tr.makePair(p, decl, p.cdr);
	st = tr.makePair(st, this, p);
        if (defs instanceof ModuleExp)
          {
	    decl.setCanRead(true);
	    // (define (f) ...) defaults f to being read-only,
	    // unless f is assigned to in this module.
	    decl.setCanWrite(true);
          }
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
	    Pair p2 = (Pair) p1.cdr;
	    if (p2.cdr == LList.Empty)
	      {
		decl = (Declaration) p1.car;
                name = decl.getName();
		value = tr.rewrite (p2.car);
	      }
	  }
      }
    if (name == null)
      return tr.syntaxError ("invalid syntax for "+getName());
    SetExp sexp = new SetExp (name, value);
    sexp.setDefining (true);
    sexp.setSetIfUnbound(true);
    if (decl != null)
      {
	sexp.binding = decl;
	if (decl.context instanceof ModuleExp
	    && decl.getCanWrite())
	  value = null;
	decl.noteValue(value);
      }
    return sexp;
  }
}
