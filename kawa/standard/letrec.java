package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * The Syntax transformer that re-writes the Scheme "letrec" primitive.
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
    int decl_count = LList.length (bindings);
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
	Object name = binding.car;
	if (! (name instanceof String)
	    && ! (name instanceof Symbol))
	  return tr.syntaxError("variable in letrec binding is not a symbol");
	if (! (binding.cdr instanceof Pair))
	  return tr.syntaxError("let has no value for '"+name+"'");
	Declaration decl = let.addDeclaration(name);
	binding = (Pair) binding.cdr;
	Object init;
	if (tr.matches(binding.car, "::"))
	  {
	    if (! (binding.cdr instanceof Pair)
		|| (binding = (Pair) binding.cdr).cdr == LList.Empty)
	      return tr.syntaxError("missing type after '::' in let");
	  }
	if (binding.cdr == LList.Empty)
	  {
	    init = binding.car;
	  }
	else if (binding.cdr instanceof Pair)
	  {
	    decl.setType(tr.exp2Type(binding));
	    decl.setFlag(Declaration.TYPE_SPECIFIED);
	    init = ((Pair) binding.cdr).car;
	  }
	else
	  return tr.syntaxError("let binding for '"+name+"' is improper list");
	inits[i] = QuoteExp.nullExp;
	orig_inits[i] = init;
	bindings = bind_pair.cdr;
      }
    tr.push(let);
    i = 0;
    for (Declaration decl = let.firstDecl();
         decl != null;  decl = decl.nextDecl(), i++)
      {
	Expression exp = tr.rewrite(orig_inits[i]);
	SetExp sexp = new SetExp(decl, exp);
	newbody[i] = sexp;
	sexp.setDefining (true);
	decl.noteValue (exp);				
      }
    newbody[decl_count] = tr.rewrite_body(body);
    let.body = new BeginExp(newbody);
    tr.pop(let);
    return let;
  }
}
