package kawa.standard;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the Scheme "and" and "or" primitives.
 * @author	Per Bothner
 */

public class and_or extends Syntax implements Printable
{
  boolean is_and;
  
  and_or (boolean is_and)
  {
    this.is_and = is_and;
  }

  public Expression rewrite (Object obj, Interpreter interp)
       throws kawa.lang.WrongArguments
  {
    if (obj == List.Empty)
      return new QuoteExp (is_and ? Interpreter.trueObject
			   : Interpreter.falseObject);
    if (! (obj instanceof Pair))
      return interp.syntaxError ("non-list arguments to and/or");
    Pair pair = (Pair) obj;
    if (pair.cdr == List.Empty)
      return interp.rewrite (pair.car);
    Expression[] inits = new Expression[1];
    LetExp let = new LetExp (inits);
    Symbol temp_name = Symbol.generate ();
    Declaration temp_decl = let.add_decl (temp_name);
    inits[0] = interp.rewrite (pair.car);
    temp_decl.noteValue (inits[0]);
    let.push (interp);
    Expression temp_exp = new ReferenceExp (temp_name, temp_decl);
    Expression rest = rewrite (pair.cdr, interp); // self-recurse
    Expression then_clause, else_clause;
    if (is_and)
      {
	then_clause = rest;
	else_clause = temp_exp;
      }
    else
      {
	then_clause = temp_exp;
	else_clause = rest;
      }
    let.body = new IfExp (temp_exp, then_clause, else_clause);
    let.pop (interp);
    return let;
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print(is_and ? "#<builtin and>" : "#<builtin or>");
  }
};
