package kawa.standard;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the Scheme "let*" primitive.
 * @author	Per Bothner
 */

public class letstar extends Syntax implements Printable
{
  static private Pattern pattern = new VarListPat (1);
  static private Pattern pattern2 = new ListPat (2);

  /**
   * Recursive utility function to re-write let* into let form.
   * @param bindings list of bindings
   * @param body the body of the "let*"
   * @param interp the evaluation context
   * @return the re-written expression
   */
  static Expression rewrite (Object bindings, Object body, Interpreter interp)
    throws WrongArguments
  {
    if (bindings == List.Empty)
      return interp.rewrite_body (body);
    if (! (bindings instanceof Pair))
      throw new WrongArguments("let*", 2, "(let* ((var init)...) body)");
    Pair bind_pair = (Pair) bindings;
    Expression[] inits = new Expression[1];
    Object[] bind_match = pattern2.match (bind_pair.car);
    if (bind_match == null)
      throw new WrongArguments("let*", 2, "(let* ((var init)...) body)");
    if (! (bind_match[0] instanceof Symbol))
      throw new WrongArguments("let*", 2, "(let* ((var init)...) body) [var is not an identifier]");
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
       throws kawa.lang.WrongArguments
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      throw new kawa.lang.WrongArguments("let*",2,"(let* bindings body)");
    return rewrite (match[0], match[1], interp);
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print("#<builtin let*>");
  }
}
