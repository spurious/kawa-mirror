package kawa.standard;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the Scheme "let*" primitive.
 * @author	Per Bothner
 */

public class letstar extends Syntax implements Printable
{
  static private Pattern pattern2 = new ListPat (2);

  /**
   * Recursive utility function to re-write let* into let form.
   * @param bindings list of bindings
   * @param body the body of the "let*"
   * @param interp the evaluation context
   * @return the re-written expression
   */
  static Expression rewrite (Object bindings, Object body, Interpreter interp)
  {
    if (bindings == List.Empty)
      return interp.rewrite_body (body);
    if (! (bindings instanceof Pair))
      return interp.syntaxError ("missing bindings in let*");
    Pair bind_pair = (Pair) bindings;
    Expression[] inits = new Expression[1];
    Object[] bind_match = pattern2.match (bind_pair.car);
    if (bind_match == null)
      return interp.syntaxError ("let* binding is not a pair");
    if (! (bind_match[0] instanceof Symbol))
      return interp.syntaxError("variable in let* binding is not a symbol");
    LetExp let = new LetExp (inits);
    Declaration decl = let.add_decl ((Symbol) bind_match[0]);
    inits[0] = interp.rewrite (bind_match[1]);
    decl.noteValue (inits[0]);
    let.push (interp);
    let.body = rewrite (bind_pair.cdr, body, interp); // self-recurse
    let.pop (interp);
    return let;
  }

  public Expression rewrite (Object obj, Interpreter interp)
  {
    if (! (obj instanceof Pair))
      return interp.syntaxError ("missing let* arguments");
    Pair pair = (Pair) obj;
    return rewrite (pair.car, pair.cdr, interp);
  }
}
