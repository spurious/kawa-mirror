package kawa.standard;
import kawa.lang.*;
import codegen.*;

/**
 * The Syntax transformer that re-writes the Scheme "letrec" primitive.
 * @author	R. Alexander Milowski
 */

public class letrec extends Syntax implements Printable
{
  static private Pattern pattern2 = new ListPat (2);
  static QuoteExp undefined_exp = new QuoteExp (Interpreter.undefinedObject);

  public Expression rewrite (Object obj, Interpreter interp)
       throws kawa.lang.WrongArguments
  {
    if (! (obj instanceof Pair))
      return interp.syntaxError ("missing letrec arguments");
    Pair pair = (Pair) obj;
    Object bindings = pair.car;
    Object body = pair.cdr;
    int decl_count = kawa.standard.length.length (bindings);
    Expression[] inits = new Expression[decl_count];
    LetExp let = new LetExp (inits);
    Expression[] newbody = new Expression[decl_count+1];
    Object[] orig_inits = new Object[decl_count];
    int i;
    for (i = 0; i < decl_count; i++)
      {
	Pair bind_pair = (Pair) bindings;
	Object[] bind_match = pattern2.match (bind_pair.car);
	if (bind_match == null)
	  return interp.syntaxError ("letrec binding is not 2-element list");
	if (! (bind_match[0] instanceof Symbol))
	  return interp.syntaxError ("letrec variable is not an indetifier");
	let.add_decl ((Symbol) bind_match[0]);
	inits[i] = undefined_exp;
	orig_inits[i] = bind_match[1];
	bindings = bind_pair.cdr;
      }
    let.push (interp);
    i = 0;
    for (Variable var = let.firstVar ();  var != null;  var = var.nextVar (), i++)
      {
	newbody[i] = new SetExp((Declaration) var,
				interp.rewrite(orig_inits[i]));
      }
    newbody[decl_count] = interp.rewrite_body(body);
    let.body = new BeginExp(newbody);
    let.pop (interp);
    return let;
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print("#<builtin letrec>");
  }
}
