package gnu.commonlisp.lang;
import kawa.lang.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.mapping.Symbol;

public class defvar extends Syntax
{
  /** True for defconst, false for defvar. */
  boolean force;

  public defvar(boolean force)
  {
    this.force = force;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    if (! (st.cdr instanceof Pair))
      return super.scanForDefinitions(st, forms, defs, tr);
    Pair p = (Pair) st.cdr;
    Object name = p.car;
    if (name instanceof String || name instanceof Symbol)
      {
	Declaration decl = defs.lookup(name);
	if (decl == null)
	  {
	    decl = new Declaration(name);
	    decl.setFlag(Declaration.IS_DYNAMIC);
	    defs.addDeclaration(decl);
	  }
	else
	  tr.error('w', "duplicate declaration for `" + name + "'");
	p = Translator.makePair(p, decl, p.cdr);
	st = Translator.makePair(st, this, p);
        if (defs instanceof ModuleExp)
          {
	    decl.setCanRead(true);
	    decl.setCanWrite(true);
          }
      }
    forms.addElement (st);
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.cdr;
    Object name = null;
    Expression value = null;
    Declaration decl = null;

    if (obj instanceof Pair)
      {
	Pair p1 = (Pair) obj;
	if (p1.car instanceof Declaration)
	  {
	    decl = (Declaration) p1.car;
	    name = decl.getSymbol();
	    if (p1.cdr instanceof Pair)
	      {
		Pair p2 = (Pair) p1.cdr;
		value = tr.rewrite (p2.car);
		if (p2.cdr != LList.Empty)
		  {
		    // Handle documentation string.  FIXME.
		  }
	      }
	    else if (p1.cdr != LList.Empty)
	      name = null;
	  }
      }
    if (name == null)
      return tr.syntaxError ("invalid syntax for "+getName());
    if (value == null)
      {
	if (force)
	  value = CommonLisp.nilExpr;
	else
	  return new QuoteExp(name);
      }
    SetExp sexp = new SetExp (name, value);
    if (! force)
      sexp.setSetIfUnbound(true);
    sexp.setDefining (true);
    if (decl != null)
      {
	sexp.setBinding(decl);
	if (decl.context instanceof ModuleExp
	    && decl.getCanWrite())
	  value = null;
	decl.noteValue(value);
      }
    return sexp;
  }

}
