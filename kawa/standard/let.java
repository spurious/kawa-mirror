package kawa.standard;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the Scheme "let" primitive.
 * This only handles standard "unnamed" let.
 * The let macro in ../lib/let.scm handles named let as well.
 * @author	Per Bothner
 */

public class let extends Syntax implements Printable
{
  static private Pattern pattern2 = new ListPat (2);

  public Expression rewrite (Object obj, Interpreter interp)
  {
    if (! (obj instanceof Pair))
      return interp.syntaxError ("missing let arguments");
    Pair pair = (Pair) obj;
    Object bindings = pair.car;
    Object body = pair.cdr;
    int decl_count = kawa.standard.length.length (bindings);
    Expression[] inits = new Expression[decl_count];
    LetExp let = new LetExp (inits);
    for (int i = 0; i < decl_count; i++)
      {
	Pair bind_pair = (Pair) bindings;
	Object[] bind_match = pattern2.match (bind_pair.car);
	if (bind_match == null)
	  return interp.syntaxError ("let binding is not a pair");
	if (! (bind_match[0] instanceof Symbol))
	  return interp.syntaxError("variable in let binding is not a symbol");
	Declaration decl = let.add_decl ((Symbol) bind_match[0]);
	inits[i] = interp.rewrite (bind_match[1]);
	decl.noteValue (inits[i]);
	bindings = bind_pair.cdr;
      }
    let.push (interp);
    let.body = interp.rewrite_body (body);
    let.pop (interp);
    return let;
  }
}
