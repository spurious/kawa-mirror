package gnu.jemacs.lang;
import kawa.lang.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.mapping.Binding;

public class defcustom extends Syntax
{
  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    if (! (st.cdr instanceof Pair))
      return super.scanForDefinitions(st, forms, defs, tr);
    Pair p = (Pair) st.cdr;
    Object name = p.car;
    if (name instanceof String || name instanceof Binding)
      {
	String sym = name.toString();
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
	if (p1.car instanceof Declaration)
	  {
	    decl = (Declaration) p1.car;
	    name = decl.getName();
	    if (p1.cdr instanceof Pair)
	      {
		Pair p2 = (Pair) p1.cdr;
		value = tr.rewrite (p2.car);
		if (p2.cdr != LList.Empty)
		  {
		    // Handle the defcustom options.  FIXME.
		  }
	      }
	    else if (p1.cdr != LList.Empty)
	      name = null;
	  }
      }
    if (name == null)
      return tr.syntaxError ("invalid syntax for "+getName());
    SetExp sexp = new SetExp (name, value);
    sexp.setDefining (true);
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
