package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * The Syntax transformer that re-writes the Scheme "letrec" primitive.
 * @author	R. Alexander Milowski
 */

public class letrec extends Syntax implements Printable
{
  static private Pattern pattern2 = new ListPat (2);

  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("missing letrec arguments");
    Pair pair = (Pair) obj;
    Object bindings = pair.car;
    Object body = pair.cdr;
    int decl_count = List.length (bindings);
    Expression[] inits = new Expression[decl_count];
    LetExp let = new LetExp (inits);
    Expression[] newbody = new Expression[decl_count+1];
    Object[] orig_inits = new Object[decl_count];
    int i;
    for (i = 0; i < decl_count; i++)
      {
	Pair bind_pair = (Pair) bindings;

	if (! (bind_pair.car instanceof Pair))
	  return tr.syntaxError ("letrec binding is not a list");
	Pair binding = (Pair) bind_pair.car;
	if (! (binding.car instanceof String))
	  return tr.syntaxError("letrec variable is not an identifier");
	String name = (String) binding.car;
	if (! (binding.cdr instanceof Pair))
	  return tr.syntaxError("let has no value for `"+name+"'");
	Declaration decl = let.addDeclaration(name);
	binding = (Pair) binding.cdr;
	Object init;
	if ("::".equals(binding.car)) // && "::" is unbound FIXME
	  {
	    if (! (binding.cdr instanceof Pair)
		|| (binding = (Pair) binding.cdr).cdr == List.Empty)
	      return tr.syntaxError("missing type after `::' in let");
	  }
	if (binding.cdr == List.Empty)
	  {
	    init = binding.car;
	  }
	else if (binding.cdr instanceof Pair)
	  {
	    decl.setType(kawa.standard.prim_method.exp2Type(binding.car, tr));
	    init = ((Pair) binding.cdr).car;
	  }
	else
	  return tr.syntaxError("let binding for `"+name+"' is improper list");
	inits[i] = QuoteExp.nullExp;
	orig_inits[i] = init;
	bindings = bind_pair.cdr;
      }
    tr.push(let);
    i = 0;
    for (Variable var = let.firstVar(); var != null; var = var.nextVar(), i++)
      {
	Expression exp = tr.rewrite(orig_inits[i]);
	Declaration decl = (Declaration) var;
	newbody[i] = new SetExp(decl, exp);
	decl.noteValue (exp);				
      }
    newbody[decl_count] = tr.rewrite_body(body);
    let.body = new BeginExp(newbody);
    tr.pop(let);
    return let;
  }
}
