package kawa.standard;
import kawa.lang.*;
import gnu.kawa.util.*;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * The Syntax transformer that re-writes the Scheme "let" primitive.
 * This only handles standard "unnamed" let.
 * The let macro in ../lib/let.scm handles named let as well.
 * @author	Per Bothner
 */

public class let extends Syntax implements Printable
{
  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("missing let arguments");
    Pair pair = (Pair) obj;
    Object bindings = pair.car;
    Object body = pair.cdr;
    int decl_count = LList.length (bindings);
    Expression[] inits = new Expression[decl_count];
    LetExp let = new LetExp (inits);
    for (int i = 0; i < decl_count; i++)
      {
	Pair bind_pair = (Pair) bindings;
	if (! (bind_pair.car instanceof Pair))
	  return tr.syntaxError ("let binding is not a pair");
	Pair binding = (Pair) bind_pair.car;
	if (! (binding.car instanceof String))
	  return tr.syntaxError("variable in let binding is not a symbol");
	String name = (String) binding.car;
	if (! (binding.cdr instanceof Pair))
	  return tr.syntaxError("let has no value for `"+name+"'");
	Declaration decl = let.addDeclaration(name);
	binding = (Pair) binding.cdr;
	Object init;
	if ("::" == binding.car) // && "::" is unbound FIXME
	  {
	    if (! (binding.cdr instanceof Pair)
		|| (binding = (Pair) binding.cdr).cdr == LList.Empty)
	      return tr.syntaxError("missing type after `::' in let");
	  }
	if (binding.cdr == LList.Empty)
	  {
	    init = binding.car;
	  }
	else if (binding.cdr instanceof Pair)
	  {
	    decl.setType(kawa.standard.prim_method.exp2Type(binding.car, tr));
	    decl.setFlag(Declaration.TYPE_SPECIFIED);
	    init = ((Pair) binding.cdr).car;
	  }
	else
	  return tr.syntaxError("let binding for `"+name+"' is improper list");
	inits[i] = tr.rewrite (init);
	decl.noteValue (inits[i]);
	bindings = bind_pair.cdr;
      }
    tr.push(let);
    let.body = tr.rewrite_body(body);
    tr.pop(let);
    return let;
  }
}
