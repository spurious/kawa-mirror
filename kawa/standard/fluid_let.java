package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * The Syntax transformer that re-writes the Scheme "fluid-let" primitive.
 * @author	Per Bothner
 */

public class fluid_let extends Syntax implements Printable
{
  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("missing let arguments");
    Pair pair = (Pair) obj;
    Object bindings = pair.car;
    Object body = pair.cdr;
    int decl_count = List.length (bindings);
    Expression[] inits = new Expression[decl_count];
    FluidLetExp let = new FluidLetExp (inits);
    for (int i = 0; i < decl_count; i++)
      {
	Pair bind_pair = (Pair) bindings;
	if (! (bind_pair.car instanceof Pair))
	  return tr.syntaxError ("fluid-let binding is not a pair");
	Pair binding = (Pair) bind_pair.car;
	if (! (binding.car instanceof String))
	  return tr.syntaxError("variable in fluid-let binding is not a symbol");
	String name = (String) binding.car;
	Declaration decl = let.addDeclaration(name);
	decl.setFluid(true);
	decl.setType(gnu.expr.FluidLetExp.typeFluidBinding);
	Expression value;
	if (binding.cdr == List.Empty)
	  value = new ReferenceExp(name);
	else if (! (binding.cdr instanceof Pair))
	  return tr.syntaxError("fluid-let has no value for `"+name+"'");
	else
	  {
	    binding = (Pair) binding.cdr;
	    if (binding.cdr != List.Empty)
	      return tr.syntaxError("let binding for `" + name
				    + "' is improper list");
	    value = tr.rewrite (binding.car);
	  }
	inits[i] = value;
	decl.noteValue (value);
	bindings = bind_pair.cdr;
      }
    tr.push(let);
    let.body = tr.rewrite_body(body);
    tr.pop(let);
    return let;
  }
}
