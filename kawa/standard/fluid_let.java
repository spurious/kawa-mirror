package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;

/**
 * The Syntax transformer that re-writes the Scheme "fluid-let" primitive.
 * @author	Per Bothner
 */

public class fluid_let extends Syntax implements Printable
{
  /** True if bindings should be evaluated sequentionally, as in ELisp let*. */
  boolean star;

  /** Value to use if an initial value is not specified.
   * Null means use the existing binding. */
  Expression defaultInit;

  public fluid_let(boolean star, Expression defaultInit)
  {
    this.star = star;
    this.defaultInit = defaultInit;
  }

  public fluid_let()
  {
    this.star = false;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("missing let arguments");
    Pair pair = (Pair) obj;
    return rewrite(pair.car, pair.cdr, tr);
  }

  public Expression rewrite (Object bindings, Object body, Translator tr)
  {
    int decl_count = star ? 1 : LList.length (bindings);
    Expression[] inits = new Expression[decl_count];
    FluidLetExp let = new FluidLetExp (inits);
    for (int i = 0; i < decl_count; i++)
      {
	Pair bind_pair = (Pair) bindings;
	String name;
	Expression value;
	Pair binding;
	if (bind_pair.car instanceof String
	    || bind_pair.car instanceof Symbol)
	  {
	    name = bind_pair.car.toString();
	    value = defaultInit;
	  }
	else if (bind_pair.car instanceof Pair
		 && ((binding = (Pair) bind_pair.car).car instanceof String
		     || binding.car instanceof Symbol))
		 
	  {
	    name = binding.car.toString();
	    if (binding.cdr == LList.Empty)
	      value = defaultInit;
	    else if (! (binding.cdr instanceof Pair)
		     || (binding = (Pair) binding.cdr).cdr != LList.Empty)
	      return tr.syntaxError("bad syntax for value of " + name
				    + " in " + getName());
	    else
	      value = tr.rewrite (binding.car);
	  }
	else
	  return tr.syntaxError("invalid " + getName() + " syntax");
	Declaration decl = let.addDeclaration(name);
	decl.setFluid(true);
	decl.setType(gnu.expr.FluidLetExp.typeFluidBinding);
	if (value == null)
	  value = new ReferenceExp(name);
	inits[i] = value;
	decl.noteValue (value);
	bindings = bind_pair.cdr;
      }
    tr.push(let);
    if (star && bindings != LList.Empty)
      let.body = rewrite (bindings, body, tr);
    else
      let.body = tr.rewrite_body(body);
    tr.pop(let);
    return let;
  }
}
