package gnu.jemacs.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.util.*;
import kawa.lang.*;
import java.util.Vector;

/**
 * The Syntax transformer that re-writes the `setq' builtin.
 * @author	Per Bothner
 */

public class setq extends Syntax implements Printable
{
  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.cdr;
    Vector results = null;
    while (obj != LList.Empty)
      {
	if (! (obj instanceof Pair))
	  return tr.syntaxError("invalid syntax for setq");
	Pair pair = (Pair) obj;
	Object sym = pair.car;
	String name;
	if (sym instanceof String)
	  name = (String) sym;
	else if (sym == ELisp.FALSE)
	  name = "nil";
	else
	  return tr.syntaxError("invalid variable name in setq");
	obj = pair.cdr;
	if (! (obj instanceof Pair))
	  return tr.syntaxError("wrong number of arguments for setq");
	pair = (Pair) obj;
	Expression value = tr.rewrite(pair.car);
	obj = pair.cdr;
	SetExp sexp = new SetExp(name, value);
	if (obj == LList.Empty)
	  {
	    sexp.setHasValue(true);
	    if (results == null)
	      return sexp;
	  }
	if (results == null)
	  results = new Vector(10);
	results.addElement(sexp);
      }
    if (results == null)
      return ELisp.nilExpr;
    Expression[] stmts = new Expression[results.size()];
    results.copyInto(stmts);
    return new BeginExp(stmts);
  }
}
