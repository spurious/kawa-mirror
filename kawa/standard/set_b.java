package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.util.*;

/**
 * The Syntax transformer that re-writes the Scheme "set!" primitive.
 * @author	Per Bothner
 */

public class set_b extends Syntax implements Printable
{

  static private Pattern pattern = new ListPat (2, 2);

  public Expression rewrite (Object obj, Translator tr)
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      return tr.syntaxError ("missing or extra arguments to set!");

    if (match[0] instanceof Pair)
      {
	// FIXME use location.rewrite_arg.
	// rewrite (set! (proc . args) rhs) => ((setter proc) rhs . args)
	// NOTE: this does not preserve evaluation order!
	Pair pair = (Pair) match[0];
	Object proc = pair.car;
	Object args = pair.cdr;
	/*
	args = new Pair(match[1], args);
	proc = new Pair("setter", new Pair(proc, LList.Empty));
	return tr.rewrite(new Pair(proc, args));
	*/

	int nargs = LList.length(args);
	Expression[] xargs = new Expression[nargs+1];
	for (int i = 1; i <= nargs; i++)
	  {
	    pair = (Pair) args;
	    xargs[i] = tr.rewrite(pair.car);
	    args = pair.cdr;
	  }
	xargs[0] = tr.rewrite(match[1]);
	return new SetApplyExp(tr.rewrite(proc), xargs);
      }

    if (! (match[0] instanceof String))
      return tr.syntaxError ("first set! argument is not a variable name");
    String sym = (String) match[0];
    Expression value = tr.rewrite (match[1]);
    Object binding = tr.environ.get (sym);
    // Hygenic macro expansion may bind a renamed (uninterned) symbol
    // to the original symbol.  Here, use the original symbol.
    if (binding != null && binding instanceof String)
      return new SetExp ((String) binding, value);
    SetExp sexp = new SetExp (sym, value);
    if (binding instanceof Declaration)
      {
	Declaration decl = (Declaration) binding;
	sexp.binding = decl;
	decl = Declaration.followAliases(decl);
	if (decl != null)
	  decl.noteValue (value);
	if (decl.getFlag(Declaration.IS_CONSTANT))
	  return tr.syntaxError ("constant variable is set!");
      }
    return sexp;
  }
}
