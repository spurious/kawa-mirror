package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * The Syntax transformer that re-writes the "define" Scheme primitive.
 * @author	Per Bothner
 */

public class define extends Syntax implements Printable
{
  boolean makePrivate;
  public define(boolean makePrivate)
  {
    this.makePrivate = makePrivate;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    String name = null;
    Expression value = null;
    Declaration decl = null;

    if (obj instanceof Pair)
      {
	Pair p1 = (Pair) obj;
	if (p1.car instanceof String && p1.cdr instanceof Pair)
	  {
	    Pair p2 = (Pair) p1.cdr;
	    if (p2.cdr == List.Empty)
	      {
		name = (String) p1.car;
		value = tr.rewrite (p2.car);
	      }
	  }
	else if (p1.car instanceof Declaration && p1.cdr instanceof Pair)
	  {
	    Pair p2 = (Pair) p1.cdr;
	    if (p2.cdr == List.Empty)
	      {
		decl = (Declaration) p1.car;
                name = decl.symbol();
		value = tr.rewrite (p2.car);
	      }
	  }
	else if (p1.car instanceof Pair)
	  {
	    Pair p2 = (Pair) p1.car;
	    if (p2.car instanceof String)
	      {
		name = (String) p2.car;
              }
            else if (p2.car instanceof Declaration)
              {
                decl = (Declaration) p2.car;
                name = decl.symbol();
              }
            if (name != null)
              {
		LambdaExp lexp = new LambdaExp();
		Lambda.rewrite(lexp, p2.cdr, p1.cdr, tr);
		lexp.setName (name);
		if (p2 instanceof PairWithPosition)
		  {
		    PairWithPosition pp = (PairWithPosition) p2;
		    lexp.setFile (pp.getFile ());
		    lexp.setLine (pp.getLine (), pp.getColumn ());
		  }
		value = lexp;
	      }
	  }
      }
    if (name == null)
      return tr.syntaxError ("invalid syntax for "+getName());
    SetExp sexp = new SetExp (name, value);
    sexp.setDefining (true);
    if (makePrivate)
      decl.setPrivate(true);
    if (decl != null)
      {
	sexp.binding = decl;
	decl.noteValue (value);
      }
    if (! (tr.currentScope() instanceof ModuleExp))
      {
        if (makePrivate)
          {
            tr.error('w', "define-private not at top level "
                     +tr.currentScope());
            return sexp;
          }

        /*
        // FIXME:  Remove following once define is handled like define-private.

	Object binding = tr.current_decls.get (name);
	// Hygenic macro expansion may bind a renamed (uninterned) symbol
	// to the original symbol.
	if (binding == null || binding instanceof String)
	  return tr.syntaxError ("invalid use of define");
	sexp.binding = (Declaration) binding;
	sexp.binding.noteValue (value);
        */
      }
    return sexp;
  }
}
