package gnu.jemacs.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.util.*;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the `defun' ELisp builtin.
 * @author	Per Bothner
 */

public class defun extends Syntax implements Printable
{
  kawa.lang.Lambda lambdaSyntax;

  public defun (kawa.lang.Lambda lambdaSyntax)
  {
    this.lambdaSyntax = lambdaSyntax;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Pair p;
    if (! (st.cdr instanceof Pair)
	|| ! ((p = (Pair) st.cdr).car instanceof String))
      return super.scanForDefinitions(st, forms, defs, tr);
    String sym = (String) p.car;
    Declaration decl = defs.lookup(sym); // FIXME use Binding2
    if (decl == null)
      {
	decl = new Declaration(sym);
	decl.setProcedureDecl(true);
	defs.addDeclaration(decl);
      }
    else
      tr.error('w', "duplicate declaration for `"+sym+"'");

    if (defs instanceof ModuleExp)
      decl.setCanRead(true);
    st = tr.makePair(st, this, tr.makePair(p, decl, p.cdr));
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
	
	if (p1.car instanceof String)
	  {
	    name = (String) p1.car;
	  }
	else if (p1.car instanceof Declaration)
	  {
	    decl = (Declaration) p1.car;
	    name = decl.getName();
	  }
	if (name != null && p1.cdr instanceof Pair)
	  {
	    Pair p2 = (Pair) p1.cdr;
	    LambdaExp lexp = new LambdaExp();
	    lambdaSyntax.rewrite(lexp, p2.car, p2.cdr, tr);
	    lexp.setName (name);
	    if (p2 instanceof PairWithPosition)
	      {
		PairWithPosition pp = (PairWithPosition) p2;
		lexp.setFile (pp.getFile ());
		lexp.setLine (pp.getLine (), pp.getColumn ());
	      }
	    value = lexp;
	    SetExp sexp = new SetExp (name, value);
	    sexp.setDefining(true);
	    sexp.setFuncDef(true);
	    if (decl != null)
	      {
		sexp.binding = decl;
		if (decl.context instanceof ModuleExp && decl.getCanWrite())
		  value = null;
		decl.noteValue(value);
	      }
	    return sexp;
	  }
      }
    return tr.syntaxError ("invalid syntax for "+getName());
  }
}
