package kawa.standard;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the Scheme "let" primitive.
 * @author	Per Bothner
 */

public class let extends Syntax implements Printable
{
  static private Pattern pattern = new VarListPat (1);
  static private Pattern pattern2 = new ListPat (2);

  public Expression rewrite (Object obj, Interpreter interp)
       throws kawa.lang.WrongArguments
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      throw new kawa.lang.WrongArguments("let",2,"(let bindings body)");
    Object bindings = match[0];
    Object body = match[1];
    int decl_count = kawa.standard.length.length (bindings);
    Expression[] inits = new Expression[decl_count];
    Declaration[] decls = new Declaration [decl_count];
    for (int i = 0; i < decl_count; i++)
      {
	Pair bind_pair = (Pair) bindings;
	Object[] bind_match = pattern2.match (bind_pair.car);
	if (bind_match == null)
	throw new WrongArguments("let", 2, "(let ((var init)...) body)");
	if (! (bind_match[0] instanceof Symbol))
	  throw new WrongArguments("let", 2, "(let ((var init)...) body) [var is not an identifier]");
	decls[i] = new Declaration ((Symbol) bind_match[0]);
	decls[i].index = i;
	inits[i] = interp.rewrite (bind_match[1]);
	bindings = bind_pair.cdr;
      }
    LetExp let = new LetExp (decls, inits);
    let.push (interp);
    let.body = interp.rewrite_body (body);
    let.pop (interp);
    return let;
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print("#<builtin let>");
  }
}
